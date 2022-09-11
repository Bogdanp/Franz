ARCH=$(shell uname -m)

APP_SRC=FranzCocoa
RKT_SRC=core
RKT_FILES=$(shell find ${RKT_SRC} -name '*.rkt')
RKT_MAIN_ZO=${RKT_SRC}/compiled/main_rkt.zo

RESOURCES_PATH=${APP_SRC}/resources
RUNTIME_NAME=runtime-${ARCH}
RUNTIME_PATH=${RESOURCES_PATH}/${RUNTIME_NAME}

CORE_ZO=${RESOURCES_PATH}/core-${ARCH}.zo

.PHONY: all
all: ${CORE_ZO} ${APP_SRC}/Backend.swift

.PHONY: clean
clean:
	rm -r ${RESOURCES_PATH}

${RKT_MAIN_ZO}: ${RKT_FILES}
	raco make -j 16 -v ${RKT_SRC}/main.rkt

${CORE_ZO}: ${RKT_MAIN_ZO}
	mkdir -p ${RESOURCES_PATH}
	rm -fr ${RUNTIME_PATH}
	raco ctool \
	  --runtime ${RUNTIME_PATH} \
	  --runtime-access ${RUNTIME_NAME} \
	  --mods $@ ${RKT_SRC}/main.rkt

${APP_SRC}/Backend.swift: ${CORE_ZO}
	raco noise-serde-codegen ${RKT_SRC}/main.rkt > $@
