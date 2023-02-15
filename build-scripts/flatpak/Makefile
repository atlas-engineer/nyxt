FLATPAK_BUILDER = flatpak-builder
BUILDER_OPTIONS = --force-clean --ccache --require-changes
TARGET_REPO = repo
APP_ID = engineer.atlas.Nyxt
MANIFEST := $(APP_ID).yaml
COMMAND = nyxt

help: ## Show help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

build: ## Build Flatpak
	@$(FLATPAK_BUILDER) $(BUILDER_OPTIONS) --repo $(TARGET_REPO) build-dir $(MANIFEST)

run: ## Run Flatpak directly
	@$(FLATPAK_BUILDER) --run build-dir $(MANIFEST) $(COMMAND)

install: ## Install Flatpak locally
	@$(FLATPAK_BUILDER) --install --user --force-clean build-dir $(MANIFEST)

clean: ## Cleanup
	@rm -rf build-dir

.PHONY: help build run install clean
