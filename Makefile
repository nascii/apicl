.DEFAULT_GOAL := all

NAME            ?= nascii
PROJECT         ?= apicl
VERSION         ?= development
ENV             ?= dev
IMAGE_NAME      ?= $(PROJECT)/$(NAME)
IMAGE_TAG       ?= latest
DOCKER_USER     ?=
DOCKER_PASSWORD ?=

NIX_BUILD_CORES ?= 2

## bindings

port_swank := 4005
port_app   := 8040
ports      := $(port_app) $(port_swank)

tmux         := tmux -2 -f $(PWD)/.tmux.conf
tmux_session := $(PROJECT)/$(NAME)

root := $(dir $(realpath $(firstword $(MAKEFILE_LIST))))
sbcl := sbcl --load $(root)/.quicklisp/setup.lisp

shell_volume_nix := nix

container_archive := container.tar.gz

## reusable and long opts for commands inside rules

shell_opts := -v $(shell_volume_nix):/nix:rw            \
	-v $(root)/.quicklisp:/root/quicklisp:rw        \
	-v $(root):/chroot                              \
	-e NIX_BUILD_CORES=$(NIX_BUILD_CORES)           \
	-w /chroot                                      \
	$(foreach v,$(ports), -p $(v):$(v) )

## macro

define fail
{ echo "error: "$(1) 1>&2; exit 1; }
endef

## rules

all: .quicklisp/setup.lisp

.quicklisp/setup.lisp: # initialize quicklisp directory
	quicklisp init

.PHONY: test
test:
	@$(sbcl) --non-interactive                                              \
		--eval '(require "asdf")'                                       \
		--eval '(push (car (directory #P".")) asdf:*central-registry*)' \
		--eval '(asdf:load-system "$(PROJECT)")'                        \
		--eval '(ql:quickload :rove)'                                   \
		--eval '(rove:run :$(PROJECT)/tests)'

.PHONY: dev/swank
dev/swank: .quicklisp/setup.lisp
	@$(sbcl)                               \
		--eval '(ql:quickload :swank)' \
		--eval '(let ((swank::*loopback-interface* "0.0.0.0")) (swank:create-server :port $(port_swank) :dont-close t))' \
		--eval '(loop while t do (sleep 1))'

.PHONY: dev/shell
dev/shell: # run development environment shell
	@docker run --rm -it                   \
		--log-driver=none              \
		$(shell_opts) nixos/nix:latest \
		nix-shell --command "exec make dev/start-session"

.PHONY: dev/session
dev/start-session: # start development environment terminals with swank server
	$(tmux) has-session    -t $(tmux_session) && $(call fail,tmux session $(tmux_session) already exists$(,) use: '$(tmux) attach-session -t $(tmux_session)' to attach) || true
	$(tmux) new-session    -s $(tmux_session) -n console -d

	$(tmux) new-window     -t $(tmux_session):1 -n swank
	$(tmux) send-keys      -t $(tmux_session):1 C-z 'make dev/swank' Enter

	$(tmux) select-window  -t $(tmux_session):0

	$(tmux) attach-session -t $(tmux_session)

.PHONY: help
help: # print defined targets and their comments
	@grep -Po '^[a-zA-Z%_/\-\s]+:+(\s.*$$|$$)' $(MAKEFILE_LIST)  \
		| sort                                               \
		| sed 's|:.*#|#|;s|#\s*|#|'                          \
		| column -t -s '#' -o ' | '
