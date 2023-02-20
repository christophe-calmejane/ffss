Publishing with old repository so it doesn't get lost.

## What is FFSS?
It's a set of specifications and implementations for a files sharing system that was designed by/for students at the Orsay (France) university during the early 2000's.

See the original description published on [the official homepage](https://ffss.sourceforge.net) (which may get lost at some point):
```
FFSS stands for Fleming File Sharing System.
It is a new powerful and easy to use file sharing protocol for LANs. It has been designed for the purpose a replacing samba (windows neighbourhood) on any small or big local networks.

FFSS somehow works like samba, that is a computer shares many folders, and a local computer connects to those shares, to retrieve or upload files.
But FFSS has many advantages to reduce the harmful effects of sharing lots of files on a pretty big network :
  - Shared files listing is hold in memory, so your HD will not be requested each time someone browse your shares. Faster, and noiseless.
  - Download and upload can be achieved using streaming (only supported mode by samba), or full file (FTP-like) using (or not, depending on your server configuration) checksum.
  - FFSS includes neteject functions : you can disable a share (no connection allowed), or set your server in silent mode (only file browsing allowed, no download).
  - FFSS server includes a FTP compatibility option (all shares are retrievable from the FTP root directory).
  - A powerful search engine is included in the FFSS master, which allow very fast file search, using many tags (video, music, pictures, doc, zip, exe...)


FFSS is composed of many applications :
  - Server : This is the part that allows you to share files.
  - Share Manager : This small application is an interface beetween the user and the server. You can add a share, remove one, change the state of the server (silent or not), eject someone from your shares, and watch file transfers.
  - Client : This is the part that browse computers, shares and files. You can download files using this application, and search for a file in the entire network using the powerful search engine.
  - Master : This part is very important, but not absolutly necessary. You usualy find a master per sub-network. A master holds a list of all the ffss server on a network, and their state (on/off/silent), so you'll not have ghost computer (like with samba). But the major functionnality of the master is it search engine, very fast and accurate, you'll find a file very quickly on the network using it. You can connect as many master as you wish, so when you search for a file, the request is forwarded to the other masters, and answers are returned back to you, for each sub-network.
  - Driver : A driver (win9x, win2k, linux), is a kernel module that creates a new file system for your computer, and allows you to browse and download fully transparently files. On windows systems a new drive letter is added to your desktop, containing all domains, servers, shares, directories and files. You can use windows explorer (drag&drop) to copy files !!


FFSS is a project in a larger plan to integrate Search Engines in the system that generate the item to be searched:
  - See the web page on Integrated Search Engines for the general idea;
  - The Project FFSS integrates the search engine to the sharing files protocol;
  - The Project Foutoir integrates the search engine to the local file system;
  - Somehow the two projects should get linked, but later....
```

## Authors
FFSS has been thought, designed, and brought to you by the following people :

- Christophe "Ze KiLleR" Calmejane:
  - Main idea, original concept, and FFSS specifications. FFSS library, master, server, shares manager and win9x driver development.
- Julien "Phoenix" Sebot :
  - Helped on FFSS specifications. Search engine core specifications.
- David "MaD" Majorel :
  - Helped on FFSS specifications.
- Jeremy "JyBy" Barbay :
  - Search engine core specifications and algorithm.
- Loic "JaL" Jaquemet :
  - FFSS linux driver development. Cvs, docs and packages maintainer.
- Benoit "Bennyben" Bourdin :
  - FFSS client (linux) development.
