#!/bin/sh

ALL=$(cut -d ' ' -f 1 <<-END
	lts-4.0   - 4.8.2.0  - 7.10.3
	lts-5.0
	lts-6.35
	lts-7.0   - 4.9.0.0  - 8.0.1
	lts-7.24
	lts-8.0   - 4.9.1.0  - 8.0.2
	lts-9.21
	lts-10.0  - 4.10.1.0 - 8.2.2
	lts-11.12
	lts-12.14 - 4.11.1.0 - 8.4.3
	lts-12.25 - 4.11.1.0 - 8.4.4
	lts-13.0  - 4.12.0.0 - 8.6.3
	lts-13.11 - 4.12.0.0 - 8.6.3
	lts-13.19 - 4.12.0.0 - 8.6.4
	lts-13.22 - 4.12.0.0 - 8.6.5
	nightly
END
)

for r in $ALL
do
  echo ">>>>> $r"
  if stack build --fast --pedantic --resolver $r
  then
    echo "<<<<<"
  else
    exit 1
  fi
done
