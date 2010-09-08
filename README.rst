PyGoWave Relaunch
=================

Hello World!

I, p2k, main developer of PyGoWave, am currently rebuilding this project from
the ground. I have decided this step, because I want to change the JavaScript
Framework to SproutCore for a far better user experience and because the
existing data structures cannot be used efficiently in combination with the
federation protocol.

**Therefore I renamed the original project to "pygowave-legacy", which is no
loger maintained, and created a new repository with the old name.**

Please delete your old forks or rename them to "pygowave-legacy" and create a
new fork of this project to keep up. All other references might also be invalid
now, please take care of them.

**Also note: It will take some time until PyGoWave is functional again. This
README file will change when it's ready to go. pygowave.net will feature the
legacy version until then.**

The plans
=========

Backend
-------

* The Django backend will be dropped; only Twisted is used from now on
* Orbited will be kept as Comet-Framework for the Web Client
* The data models will be completely redesigned to fit the Federation Protocol
  requirements; all data will be XML
* There will also be an Erlang port of the RPC server as before


Protocols
---------

* There will be means to convert operations and snapshots of waves for use with
  several protocols with a modular approach
* The old PyGoWave protocol will be replaced by the official Wave Data Protocol
  with a smaller extension to allow subscribing to events and letter-by-letter
  updates (this will be called "PyGoWave Simple Data Protocol")
* The official Wave Robot Protocol will be fully supported
* For clients other than the Web Client, there will be a bigger extension to the
  Wave Data Protocol which allows the reception of XML-operations (called
  "PyGoWave Extended Data Protocol")
* The client APIs, which will be available for lots of programming languages and
  platforms, will also come in two tastes: simple and extended
* The extended API features both raw XML data manipulation as well as wrapped
  Wave/Wavelet/Blip models driven by the PyGoWave Extended Data Protocol
* The simple API features Wave/Wavelet/Blip models only


Frontend / Web Client
---------------------

* The complete Web Client will be written with SproutCore, a versatile
  JavaScript framework which allows to write desktop-quality GUI applications
  directly in JavaScript
* The pure JavaScript code of Orbited will be ported to SproutCore as well as
  the STOMP protocol which is used as underlying transport for message bundles
* The new frontend tries to mimic the behaviour of the official Wave web
  frontend as good as possible or even better


Code sharing
------------

* A new sub-project will be spawned to automatically convert the python code of
  the core algorithms to a number of other programming languages resp.
  frameworks
* In detail, the PyGoWave APIs will be available for: Python, Erlang,
  JavaScript/SC, C++/Qt, ObjC/NeXT, C#/.NET, Java, PHP and possibly others at a
  later point
* This converter will only translate a small subset of Python, though
