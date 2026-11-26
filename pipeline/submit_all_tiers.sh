#!/bin/bash
# ---------------------------------------------------------
# START THE COMPLETE BF-NEE RUN (on the login node):
#
#   bash submit_all_tiers.sh
#
# 1. Tier 0: all countries except the big ones, small memory
# 2. After tier 0 has finished, a controller job checks which
#    countries failed because of memory / time and submits
#    them to tier 1 (more memory), and so on.
# 3. The big countries (big_country_iso_list.txt) run in the
#    last tier.
# 4. After the last tier, a final report is written to
#    tier_lists/final_report_<run>.csv
#
# Countries that are already finished with the v3
# pipeline are skipped. Use  bash submit_all_tiers.sh --all
# to include them anyway.
# ---------------------------------------------------------

set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

source "${THIS_DIR}/tiers.conf"
source "${SCRIPT_DIR}/pipeline_tiers_common.sh"

INCLUDE_DONE="no"
if [[ "${1:-}" == "--all" ]]; then
  INCLUDE_DONE="yes"
fi

# ---------------------------------------------------------
# RESUME MODE: start one specific tier with one ISO list
#   bash submit_all_tiers.sh --resume <tier> <iso_list_file>
# e.g. only the big countries in the last tier:
#   bash submit_all_tiers.sh --resume 2 big_country_iso_list.txt
# Countries already finished are removed from the list.
# The usual controller (escalation + final report) follows.
# ---------------------------------------------------------
if [[ "${1:-}" == "--resume" ]]; then
  RESUME_TIER="${2:?tier number missing}"
  RESUME_LIST_IN="$(readlink -f "${3:?ISO list missing}")"
  
  mkdir -p "${LIST_DIR}" "${LOG_DIR}" "${STATUS_DIR}"
  
  RUN_ID=$(date +%Y%m%d_%H%M%S)
  echo "${RUN_ID}" > "${LIST_DIR}/current_run_id.txt"
  
  RESUME_LIST="${LIST_DIR}/run_${RUN_ID}_tier${RESUME_TIER}.txt"
  : > "${RESUME_LIST}"
  
  while read -r ISO3; do
    is_country_done "${ISO3}" || echo "${ISO3}" >> "${RESUME_LIST}"
  done < <(read_iso_list "${RESUME_LIST_IN}")
  
  echo "Resume run ${RUN_ID}: tier ${RESUME_TIER} with $(wc -l < "${RESUME_LIST}") countries:"
  tr '\n' ' ' < "${RESUME_LIST}"; echo
  
  SUBMIT_RC=0
  submit_tier "${RESUME_TIER}" "${RESUME_LIST}" || SUBMIT_RC=$?
  [[ ${SUBMIT_RC} -eq 0 ]] || { echo "Nothing submitted."; exit 1; }
  exit 0
fi

mkdir -p "${LIST_DIR}" "${LOG_DIR}" "${STATUS_DIR}"

if [[ "${SKIP_PREFLIGHT:-0}" != "1" ]]; then
  if ! preflight_tiers; then
    echo "ERROR: at least one tier cannot be submitted. Adjust partition/mem/time in tiers.conf."
    echo "(Skip this check with SKIP_PREFLIGHT=1 bash submit_all_tiers.sh)"
    exit 1
  fi
fi

for f in job_all_countries.job tier_controller.job run_country_by_iso_pipeline.R; do
  if [[ ! -f "${SCRIPT_DIR}/${f}" ]]; then
    echo "ERROR: missing ${SCRIPT_DIR}/${f}"
    exit 1
  fi
done

if [[ ! -f "${ALL_ISO_LIST}" ]]; then
  echo "ERROR: ISO list not found: ${ALL_ISO_LIST}"
  exit 1
fi

N_TIERS=${#TIER_MEM[@]}

if [[ ${#TIER_PARTITION[@]} -ne ${N_TIERS} || ${#TIER_TIME[@]} -ne ${N_TIERS} || ${#TIER_PARALLEL[@]} -ne ${N_TIERS} ]]; then
  echo "ERROR: tier arrays in tiers.conf have different lengths."
  exit 1
fi

# ---------------------------------------------------------
# RUN ID (all list files of this run share it)
# ---------------------------------------------------------
RUN_ID=$(date +%Y%m%d_%H%M%S)
echo "${RUN_ID}" > "${LIST_DIR}/current_run_id.txt"

# ---------------------------------------------------------
# BUILD TIER-0 LIST = all countries - big countries - done
# ---------------------------------------------------------
BIG=$(read_iso_list "${BIG_ISO_LIST}")

declare -A IS_BIG=()
while read -r ISO3; do
  [[ -n "${ISO3}" ]] && IS_BIG["${ISO3}"]=1
done <<< "${BIG}"

# medium countries start in a later tier (see tiers.conf)
if [[ ${MEDIUM_START_TIER:-0} -gt 0 && -f "${MEDIUM_ISO_LIST:-}" ]]; then
  while read -r ISO3; do
    [[ -n "${ISO3}" ]] && IS_BIG["${ISO3}"]=1
  done < <(read_iso_list "${MEDIUM_ISO_LIST}")
fi

TIER0_LIST="${LIST_DIR}/run_${RUN_ID}_tier0.txt"
: > "${TIER0_LIST}"

N_SKIPPED_DONE=0

while read -r ISO3; do
  if [[ -n "${IS_BIG[${ISO3}]:-}" ]]; then
    continue
  fi
  if [[ "${INCLUDE_DONE}" == "no" ]] && is_country_done "${ISO3}"; then
    N_SKIPPED_DONE=$((N_SKIPPED_DONE + 1))
    continue
  fi
  echo "${ISO3}" >> "${TIER0_LIST}"
done < <(read_iso_list "${ALL_ISO_LIST}")

echo "=================================================="
echo "BF-NEE tiered run ${RUN_ID}"
echo "Tiers: ${N_TIERS}"
for ((t = 0; t < N_TIERS; t++)); do
  echo "  tier ${t}: ${TIER_PARTITION[$t]}, ${TIER_MEM[$t]}, ${TIER_TIME[$t]}, max ${TIER_PARALLEL[$t]} parallel"
done
echo "Countries in tier 0: $(wc -l < "${TIER0_LIST}")"
echo "Medium countries (start in tier ${MEDIUM_START_TIER:-0}): $(read_iso_list "${MEDIUM_ISO_LIST:-/dev/null}" | grep -c . || true)"
echo "Big countries (last tier): $(echo "${BIG}" | grep -c . || true)"
echo "Already finished, skipped: ${N_SKIPPED_DONE}"
echo "Escalation mode: ${ESCALATE_MODE}"
echo "=================================================="

# ---------------------------------------------------------
# SUBMIT
# If tier 0 is empty, the controller logic is started
# directly so the big countries still run.
# ---------------------------------------------------------
SUBMIT_RC=0
submit_tier 0 "${TIER0_LIST}" || SUBMIT_RC=$?

if [[ ${SUBMIT_RC} -eq 2 ]]; then
  echo "ERROR: tier 0 could not be submitted."
  exit 1
fi

if [[ ${SUBMIT_RC} -eq 1 ]]; then
  echo "Tier 0 is empty -> handing over to controller directly."
  bash "${SCRIPT_DIR}/tier_controller.job" 0 "none" "${TIER0_LIST}"
fi

echo "Monitor with:  squeue -u \$USER"
echo "Final report:  ${LIST_DIR}/final_report_${RUN_ID}.csv"
