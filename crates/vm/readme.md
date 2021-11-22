# Inkstone-VM

`inkstone-vm` contains the implementation of a simple virtual machine that runs [bytecode][].

[bytecode]: ../inkstone-bytecode/readme.md

## Notes

The VM is basically two things glued together:

- An event loop which runs until its root function returns or yields, and
- An instruction handler which reads in an instruction, manipulates the stack, and spits out an action to perform (if any).
