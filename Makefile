export LSP_USE_PLISTS := true

DRONES_DIR = $(shell git config "borg.drones-directory" || echo "site-lisp")

-include $(DRONES_DIR)/borg/borg.mk

EMACS_EXTRA += -L core -L build \
	--eval "(startup-redirect-eln-cache \".local/cache/eln/\")" \
	--load dream-autoloads-build --load dream-compile

bootstrap-borg:
		@git submodule--helper clone --name borg --path $(DRONES_DIR)/borg \
		--url https://github.com/emacscollective/borg.git
		@cd $(DRONES_DIR)/borg; git symbolic-ref HEAD refs/heads/main
		@cd $(DRONES_DIR)/borg; git reset --hard HEAD

.PHONY: autoloads runtime-artifacts config-build config-native check check-isolated check-declare runtime-no-compile smoke benchmark-baseline benchmark

autoloads:
	$(Q)$(EMACS_BATCH) $(BORG_ARGS) --funcall dream-build-autoloads 2>&1

runtime-artifacts:
	$(Q)$(EMACS_BATCH) $(BORG_ARGS) --funcall dream-build-runtime-artifacts 2>&1

config-build:
	$(Q)$(EMACS_BATCH) $(BORG_ARGS) --eval "(dream-build-config nil)" 2>&1

config-native:
	$(Q)$(EMACS_BATCH) $(BORG_ARGS) --eval "(dream-build-config t)" 2>&1

check:
	$(Q)$(EMACS) -Q --batch -l test/run-tests.el

check-isolated:
	$(Q)$(EMACS_BATCH) $(BORG_ARGS) --funcall dream-build-check-isolated 2>&1

check-declare:
	$(Q)$(EMACS_BATCH) $(BORG_ARGS) --funcall dream-build-check-declare 2>&1

runtime-no-compile:
	$(Q)$(EMACS) -Q --batch -l test/dream-runtime-smoke.el

smoke:
	$(Q)$(EMACS) -Q --batch -l early-init.el -l init.el \
		--eval "(princ (format \"Dream smoke: %s features, GC %s/%s\\n\" (length features) gc-cons-threshold gc-cons-percentage))"

benchmark-baseline:
	$(Q)$(EMACS) -Q --batch -l test/dream-benchmark.el --eval "(dream-benchmark-run t)"

benchmark:
	$(Q)$(EMACS) -Q --batch -l test/dream-benchmark.el --eval "(dream-benchmark-run nil)"
