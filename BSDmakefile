.PHONY all
all:
	@which gmake > /dev/null 2>&1 ||\
		(echo "GNU make (gmake) is required." && exit 1)
	@gmake ${.MAKEFLAGS} ${.TARGETS}
