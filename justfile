format:
	fd -e hs | xargs -P $(nproc) -n 1 fourmolu -q -i
