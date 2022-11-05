LISP ?= sbcl

build:
	$(LISP) --load rhombihexadeltille.asd \
		--eval "(ql:quickload :deploy)" \
		--eval "(ql:quickload :rhombihexadeltille)" \
		--eval "(asdf:make :rhombihexadeltille)" \
		--eval "(quit)"
