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

root := $(dir $(realpath $(firstword $(MAKEFILE_LIST))))

port_swank := 4005
port_app   := 8040
ports      := $(port_app) $(port_swank)

tmux         := tmux -2 -f $(PWD)/.tmux.conf
tmux_session := $(PROJECT)/$(NAME)

shell_volume_nix  := nix
container_archive := container.tar.gz

## make-specific "fixes" to allow passing some values to functions/assignments

comma := ,
dash  := \#

## reusable and long opts for commands inside rules

sbcl := sbcl $(shell if ! [ -t 0 ]; then echo --non-interactive; fi)  \
	--load $(root)/.quicklisp/setup.lisp                          \
	--eval '(require "asdf")'                                     \
	--eval '(push (car (directory ".")) asdf:*central-registry*)' \
	--eval '(asdf:load-system "$(PROJECT)")'

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

all: .quicklisp/setup.lisp # prepare dependencies and build application

.quicklisp/setup.lisp: # initialize quicklisp directory
	quicklisp init

.PHONY: run
run: # run application
	@$(sbcl) --eval '($(PROJECT):run)'

.PHONY: test
test: # run application tests
	$(sbcl)                                       \
		--eval '(ql:quickload :rove)'         \
		--eval '(rove:run :$(PROJECT)/tests)' \
		--eval '(quit)'

.PHONY: dev/swank
dev/swank: .quicklisp/setup.lisp # run swank server for slime
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
	$(tmux) has-session    -t $(tmux_session) && $(call fail,tmux session $(tmux_session) already exists$(comma) use: '$(tmux) attach-session -t $(tmux_session)' to attach) || true
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
