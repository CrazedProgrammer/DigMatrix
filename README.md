# DigMatrix

DigMatrix is a distributed mining program for ComputerCraft using Plethora.

* The server keeps track of all scanned blocks.
* The program on the player's neural interface uses the block scanner to scan for blocks, which get uploaded to the REST server.
* The program on the turtles read the blocks from the REST server and start mining it. It uses GPS to keep track of the location.
