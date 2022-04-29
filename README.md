## What is this for?

Running IWFM is a challenge. Normally the process requires a number of steps to get your own
machine ready to run the executables, and run the steps by hand. This repository contains
everything that is needed to run it, and you don't even need to check this repository out:
the build image is published to [Docker Hub](https://hub.docker.com/r/ashesfall/iwfm-base).

The fact that the model is now a self-contained image means that it can be run on any public
cloud as well, so it isn't even necessary for you to tie up your own machine to do it.


## Running the Integrated Water Flow Model

Running IWFM is now a single step (after downloading docker, which is free for most users).
You'll first want to identify the model data archive you want to use. This can be found on
the California Water Department website, and as of this writing the latest data is available
at https://data.cnra.ca.gov/dataset/31f3ddf8-752a-4b04-99e0-2a9f9139817a/resource/bc00cfa5-86ac-4e95-acda-6df1f3d85a73/download/c2vsimfg_version1.01.zip

All you have to do is supply this location to the simulator image, and it will complete the
process.

```
docker run -p 8080:80 -e IWFM_MODEL=https://data.cnra.ca.gov/dataset/31f3ddf8-752a-4b04-99e0-2a9f9139817a/resource/bc00cfa5-86ac-4e95-acda-6df1f3d85a73/download/c2vsimfg_version1.01.zip -it ashesfall/iwfm-base
```

The -p option here is what allows you to access the results of the simulation. By binding the
container's built in web server to port 8080 of your own machine you can simply bring up the
site http://localhost:8080/files in your browser to keep an eye on any output it generates.
These pages are updated in real time, so you can monitor the entire process using just a browser.

## Deploying the Simulation to Azure

Terraform scripts are provided here to allow any Microsoft user to perform the same operation in
their Azure Cloud account. Because all the results are accessible via the browser, there is no
need to even deal with the Azure console, you can monitor the process from anywhere.

First you'll need to download the Azure CLI and Terraform, if you have not already. They are both
free, and supported on Windows, Linux, and Mac (Intel/ARM).

[Azure CLI](https://docs.microsoft.com/en-us/cli/azure/)

[Terraform](https://learn.hashicorp.com/tutorials/terraform/install-cli)

Next you'll need to log in to your Azure account:

```
az login
```

This will open a browser where you can complete the normal sign in process.

Then you can initialize terraform, and apply the template. Make sure to specify the ID of the Azure
subscription you want to use. Make sure to run these commands from the azure directory located
within where ever you checked out this GitHub repository.

```
terraform init
terraform apply -var subscription_id=your_azure_subscription_id
```


## Building New Versions of the IWFM Codebase

The IWFM code is included here. If you want to make changes to it, you can check out this
repository, make your changes, and build a new image.

```
docker build . -t iwfm
```

Then you can run your new container in a similar way to above. Notice the new image name "iwfm".

```
docker run -p 8080:80 -e IWFM_MODEL=https://data.cnra.ca.gov/dataset/31f3ddf8-752a-4b04-99e0-2a9f9139817a/resource/bc00cfa5-86ac-4e95-acda-6df1f3d85a73/download/c2vsimfg_version1.01.zip -it iwfm
```
