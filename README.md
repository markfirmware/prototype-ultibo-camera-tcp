Ultibo camera stream over tcp

Using these repos:

    git subtree add -Prepo/pik33/SimpleCamera https://github.com/pik33/SimpleCamera master --squash
    git subtree add -Prepo/pjde-ultibo/openmax https://github.com/pjde-ultibo/openmax master --squash

Use Ultibo Lazarus to build device.lpi. It is set for Raspberry Pi 3B or 3B+. Copy config.txt and kernel7.img to an empty ultibo sd card. The sd card will also need start_x.elf, fixup_x.dat and bootcode.bin.

The client program uses stock Lazarus on the host. Install synapse and graphics32 into the host Lazarus.

Compile ImagingTesterProgram.lpi which is a test client.
You will need to modify the -Fu paths in project options to reflect where you put synapse and graphics32.
Run the client and enter the ultibo ip address. You should see images from the camera.
