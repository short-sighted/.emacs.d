DRONES_DIR = $(shell git config "borg.drones-directory" || echo "site-lisp")

-include $(DRONES_DIR)/borg/borg.mk

EMACS_EXTRA += -L core --load dream-autoloads

bootstrap-borg:
		@git submodule--helper clone --name borg --path $(DRONES_DIR)/borg \
		--url https://github.com/emacscollective/borg.git
		@cd $(DRONES_DIR)/borg; git symbolic-ref HEAD refs/heads/main
		@cd $(DRONES_DIR)/borg; git reset --hard HEAD

.PHONY: autoloads

autoloads:
	$(Q)$(EMACS_BATCH) $(BORG_ARGS) \
	--eval "(dream-autoloads-generate)" 2>&1
