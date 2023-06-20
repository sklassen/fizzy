
release:
	rebar3 release

install: release
	cp -r _build/default/rel/fizzy/* ${DESTDIR}

snapcraft:
	snapcraft --destructive-mode --verbose

test:
	sudo snap install ./fizzy_0.0.1_amd64.snap --dangerous

clean:
	rm -rf parts stage prime
