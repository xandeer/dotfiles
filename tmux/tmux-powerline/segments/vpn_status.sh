# Prints the VPN status. The result is cached and updated according to $update_period.
TMUX_POWERLINE_SEG_VPN_NAME_DEFAULT="vpn"

run_segment() {
	local tmp_file="${TMUX_POWERLINE_DIR_TEMPORARY}/vpn_status.txt"
	local vpn_status

	if [ -f "$tmp_file" ]; then
		if shell_is_osx; then
			stat >/dev/null 2>&1 && is_gnu_stat=false || is_gnu_stat=true
			if [ "$is_gnu_stat" == "true" ];then
				last_update=$(stat -c "%Y" ${tmp_file})
			else
				last_update=$(stat -f "%m" ${tmp_file})
			fi
		fi

		time_now=$(date +%s)
		update_period=5
		up_to_date=$(echo "(${time_now}-${last_update}) < ${update_period}" | bc)

		if [ "$up_to_date" -eq 1 ]; then
			vpn_status=$(cat ${tmp_file})
		fi
	fi

	if [ -z "$vpn_status" ]; then
    vpn_status=$(scutil --nc status ${TMUX_POWERLINE_SEG_VPN_NAME_DEFAULT} | sed -n 1p)
		#vpn_status=$(scutil --nc status vpn | sed -n 1p)

		if [ "$?" -eq "0" ]; then
			echo "${vpn_status}" > $tmp_file
		elif [ -f "${tmp_file}" ]; then
			vpn_status=$(cat "$tmp_file")
		fi
	fi

	if [ -n "$vpn_status" ]; then
		echo " ${vpn_status}"
	fi

	return 0
}
