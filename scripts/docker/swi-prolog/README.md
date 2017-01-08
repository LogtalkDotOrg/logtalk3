
Logtalk Docker configuration
============================

Building a Docker image
-----------------------

#### Nightly build

docker build -t="logtalk/logtalk:nightly" .

#### Last stable release

docker build --build-arg LOGTALK_VERSION=lgt3090stable -t="logtalk/logtalk:lgt3090stable" -t="logtalk/logtalk:latest" .




Running a container
-------------------

#### From the nightly build image

docker run -it --name logtalk_nightly "logtalk/logtalk:nightly"


#### From the last stable release

docker run -it --name logtalk_latest "logtalk/logtalk:latest"

