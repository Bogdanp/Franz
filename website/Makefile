CROSS_VERSION=8.7
CROSS_PREFIX=raco cross --version ${CROSS_VERSION} --target x86_64-linux

.PHONY: serve
serve:
	python -m http.server

.PHONY: deploy
deploy: atom.xml manual/index.html
	rsync \
		--exclude Makefile \
		--exclude api \
		--exclude api.rkt \
		--exclude api_dist \
		--exclude feed.rkt \
		-avh . franz:~/www/

.PHONY: deploy_api
deploy_api: api_dist/bin/api
	rsync -avh api_dist/ franz:~/api/
	ssh racksnaps "sudo -S systemctl restart franz-api"

manual/index.html: ../manual/*.scrbl
	raco scribble --html --dest manual +m ../manual/index.scrbl

atom.xml: versions/changelog.txt
	racket feed.rkt

.PHONY: cross_setup
cross_setup:
	${CROSS_PREFIX} pkg install -D --skip-installed at-exp-lib threading-lib web-server-lib

api: api.rkt
	${CROSS_PREFIX} exe -o api api.rkt

api_dist/bin/api: api
	${CROSS_PREFIX} dist api_dist api
