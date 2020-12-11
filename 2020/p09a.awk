#!/usr/bin/env awk -f

BEGIN {wsz = 25}
{
	nums[NR] = $1
	if (NR > wsz) {
		found = 0
		for (i=NR-wsz; !found && i < NR; i++)
			for (j=i+1; !found && j < NR; j++)
				if (nums[i] != nums[j] && nums[i] + nums[j] == $1)
					found = 1
		if (!found) {
			print $1 # 258585477
			exit
		}
	}
}