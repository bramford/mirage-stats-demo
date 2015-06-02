# mirage-stats-demo

Unikernel that displays stats on a web page. Currently very basic and
experimental.

### Instructions

You'll need the dev versions of some mirage libraries:

```
opam repo add mirage-dev https://github.com/mirage/mirage-dev.git
```

Then you can compile the app into a Xen image:

```
mirage configure --xen
make depend
make
# edit www.xl to uncomment the vif line
sudo xl create -c www.xl
```

By default, it will use IP=10.0.0.2 and GW=10.0.0.1. You can change these
settings by doing:

```
IP=<ip> GW=<ip> mirage configure --xen
```
