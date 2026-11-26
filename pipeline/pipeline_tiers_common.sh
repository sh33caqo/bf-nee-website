#!/bin/bash
# ---------------------------------------------------------
# SHARED FUNCTIONS FOR submit_all_tiers.sh AND tier_controller.job
# (sourced, not run directly)
# ---------------------------------------------------------

# ---------------------------------------------------------
# READ ISO LIST (strip Windows line endings, blanks, case)
# ---------------------------------------------------------
read_iso_list () {
  local f="$1"
  [[ -f "${f}" ]] || return 0
  tr -d '\r' < "${f}" | tr '[:lower:]' '[:upper:]' | sed 's/[[:space:]]//g' | grep -v '^$' || true
}

# ---------------------------------------------------------
# IS A COUNTRY FINISHED WITH THE v3 PIPELINE?
# last status entry of step 08 v3 = success AND master table
# contains the new agri-constrained column
# ---------------------------------------------------------
is_country_done () {
  local iso3="$1"
  local iso3_lower
  iso3_lower=$(echo "${iso3}" | tr '[:upper:]' '[:lower:]')

  local status_file="${STATUS_DIR}/pipeline_status_${iso3}.csv"
  local master_file="${OUT_BASE}/${iso3}/BF_NEE_master_country_commodity_ecosystem_${iso3_lower}_all_years.csv"

  [[ -f "${status_file}" ]] || return 1

  local last_entry
  last_entry=$(grep '"08_calculating_indicator_v3",' "${status_file}" | tail -n 1 || true)

  echo "${last_entry}" | grep -q '"08_calculating_indicator_v3","success"' || return 1

  [[ -s "${master_file}" ]] || return 1

  head -n 1 "${master_file}" | grep -q "natural_loss_area_pasture" || return 1

  return 0
}

# ---------------------------------------------------------
# LAST ERROR MESSAGE IN THE STATUS FILE OF A COUNTRY
# ---------------------------------------------------------
last_status_error () {
  local iso3="$1"
  local status_file="${STATUS_DIR}/pipeline_status_${iso3}.csv"
  [[ -f "${status_file}" ]] || { echo "no_status_file"; return 0; }
  grep '"failed"' "${status_file}" | tail -n 1 | tr -d '\r' || true
}

# ---------------------------------------------------------
# SUBMIT ONE TIER + ITS CONTROLLER
# usage: submit_tier <tier> <iso_list_file> [retry]
# ---------------------------------------------------------
submit_tier () {
  local tier="$1"
  local list_file="$2"
  local retry="${3:-0}"

  local n
  n=$(read_iso_list "${list_file}" | wc -l)

  if [[ "${n}" -eq 0 ]]; then
    echo "Tier ${tier}: no countries to run."
    return 1
  fi

  local partition="${TIER_PARTITION[$tier]}"
  local mem="${TIER_MEM[$tier]}"
  local time_limit="${TIER_TIME[$tier]}"
  local parallel="${TIER_PARALLEL[$tier]}"

  local array_id
  array_id=$(sbatch --parsable \
    --job-name="bfnee_tier${tier}" \
    --partition="${partition}" \
    --mem="${mem}" \
    --time="${time_limit}" \
    --cpus-per-task=1 \
    --array="1-${n}%${parallel}" \
    --output="${LOG_DIR}/bfnee_tier${tier}_%A_%a.out" \
    --error="${LOG_DIR}/bfnee_tier${tier}_%A_%a.err" \
    --export=ALL,ISO_LIST="${list_file}",FORCE_STEPS="${FORCE_STEPS}" \
    "${SCRIPT_DIR}/job_all_countries.job" | cut -d ';' -f 1 || true)

  if [[ ! "${array_id}" =~ ^[0-9]+$ ]]; then
    echo "ERROR: submission of tier ${tier} failed (partition ${partition}, ${mem})." >&2
    echo "Countries of this tier are listed in: ${list_file}" >&2
    return 2
  fi

  echo "Tier ${tier}: submitted ${n} countries as job ${array_id} (${partition}, ${mem}, ${time_limit}, max ${parallel} parallel)"

  # Controller starts when ALL array tasks have ended (any state)
  local ctrl_id
  ctrl_id=$(sbatch --parsable \
    --job-name="bfnee_ctrl${tier}" \
    --partition="${TIER_PARTITION[0]}" \
    --mem=2G \
    --time=01:00:00 \
    --cpus-per-task=1 \
    --dependency="afterany:${array_id}" \
    --output="${LOG_DIR}/bfnee_controller_tier${tier}_%j.out" \
    --error="${LOG_DIR}/bfnee_controller_tier${tier}_%j.err" \
    "${SCRIPT_DIR}/tier_controller.job" "${tier}" "${array_id}" "${list_file}" "${retry}" | cut -d ';' -f 1 || true)

  if [[ ! "${ctrl_id}" =~ ^[0-9]+$ ]]; then
    echo "ERROR: controller for tier ${tier} could not be submitted. Tier job ${array_id} still runs." >&2
    return 2
  fi

  echo "Tier ${tier}: controller job ${ctrl_id} waits for job ${array_id}"

  return 0
}

# ---------------------------------------------------------
# PREFLIGHT: can every tier be submitted at all?
# (partition permission, memory and time limits)
# ---------------------------------------------------------
preflight_tiers () {
  local ok=0
  local t
  for ((t = 0; t < ${#TIER_MEM[@]}; t++)); do
    if sbatch --test-only --partition="${TIER_PARTITION[$t]}" --mem="${TIER_MEM[$t]}" \
         --time="${TIER_TIME[$t]}" --cpus-per-task=1 --wrap="hostname" >/dev/null 2>/tmp/bfnee_preflight_$$; then
      echo "Preflight tier ${t}: OK (${TIER_PARTITION[$t]}, ${TIER_MEM[$t]}, ${TIER_TIME[$t]})"
    else
      echo "Preflight tier ${t}: NOT POSSIBLE (${TIER_PARTITION[$t]}, ${TIER_MEM[$t]}, ${TIER_TIME[$t]})"
      sed 's/^/    /' /tmp/bfnee_preflight_$$
      ok=1
    fi
  done
  rm -f /tmp/bfnee_preflight_$$
  return ${ok}
}
