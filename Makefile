OPAM=opam
OPAMX=$(OPAM) exec --
DUNE=$(OPAMX) dune
WHICHX=$(DUNE) exec -- which 

YARN=yarn

DYLIB_LIST_FILE=packaging/macOS_dylibs.txt
PACKAGING_BIN=$(DUNE) exec -- packaging/package.exe
BFA_C_BIN=_build/install/default/bin/bfa-c
VSCODE_BC_JS=vscode/src/bfa_vscode.bc.js

PACKAGE_DIST=package
VSCODE_DIST=dist

##### Normal ocaml stuff #####
.PHONY: ocaml
ocaml:
	 $(DUNE) build

.PHONY: ocaml-test
ocaml-test:
	$(DUNE) test
	
	
##### Packaging bfa-c #####

# From inside the package folder one can run:
# BFA_Z3_PATH=./bin/z3 DYLD_LIBRARY_PATH=./lib:$DYLD_LIBRARY_PATH ./bin/bfa-c exec-main file.c
.PHONY: package
package: ocaml packaging/bin-locations.txt packaging/macOS_dylibs.txt
	$(DUNE) build @dylist-file
	$(PACKAGING_BIN) copy-files $(DYLIB_LIST_FILE) $(PACKAGE_DIST)/lib
	$(PACKAGING_BIN) copy-files packaging/bin-locations.txt $(PACKAGE_DIST)/bin
	mkdir -p $(PACKAGE_DIST)/lib
	$(PACKAGING_BIN) copy-cerb-runtime $(PACKAGE_DIST)/lib/cerb_runtime
	

packaging/bin-locations.txt:
	$(WHICHX) bfa-c > $@
	$(WHICHX) z3 >> $@

packaging/macOS_dylibs.txt:
	$(PACKAGING_BIN) infer-dylibs $(BFA_C_BIN) > $@

##### Switch creation / dependency setup #####

.PHONY: switch
switch:
	$(OPAM) switch create . ocaml-base-compiler.5.3.0 --deps-only --with-test --with-doc -y
	$(OPAM) install ocaml-lsp-server odig ocamlformat -y
	
.PHONY: ocaml-deps
ocaml-deps:
	$(OPAM) install . --deps-only --with-test --with-doc

##### JavaScript stuff #####

.PHONY: npm-deps
npm-deps:
	$(YARN) install --immutable
	
.PHONY: vscode-reinstall-dev
vscode-reinstall-dev: vscode-package
	$(YARN) install-ext
		
	
.PHONY: vscode-package
vscode-package: vscode
	$(YARN) package
	
.PHONY: vscode
vscode: js-bundle


.PHONY: js-bundle
js-bundle: vscode-ocaml
	$(YARN) compile

.PHONY: vscode-ocaml
vscode-ocaml:
	$(DUNE) build $(VSCODE_BC_JS)
	
.PHONY: for-local
for-local: ocaml vscode
	
.PHONY: clean
clean:
	$(DUNE) clean
	rm -rf $(PACKAGE_DIST)
	rm -rf $(VSCODE_DIST)
	rm -rf packaging/bin-locations.txt packaging/macOS_dylibs.txt
	rm -f bfa-vscode.vsix