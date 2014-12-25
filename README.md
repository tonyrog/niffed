You been niffed
===============

niffed is a wrapper to let you use the nif API inside an Erlang driver.
Is may even be possible to load the same code as both a driver and
a nif at the same time.

The nif calls are made through a simple dispatcher via the drivers
control interface.

This make porting nif code into a driver or vice versa a bit simpler.




