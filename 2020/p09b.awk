#!/usr/bin/env awk -f

BEGIN {target = 258585477; ws = 1}
{
	nums[NR] = $1
	cnt += $1
	while(cnt > target) {
		cnt -= nums[ws]
		ws++;
	}
	if (cnt == target) {
		min=nums[ws]
		max=min
		for (i=ws; i <= NR; i++) {
			if (nums[i] < min) min = nums[i]
			if (nums[i] > max) max = nums[i]
		}
		print min+max # 36981213
		exit
	}
}