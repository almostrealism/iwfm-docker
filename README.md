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

## Deploying the Simulation to AWS

First you'll need to download Terraform, if you have not already. It is free, and available on
Windows, Linux, and Mac (Intel/ARM).

[Terraform](https://learn.hashicorp.com/tutorials/terraform/install-cli)

Next, navigate to the "terraform/iwfm-aws" directory of this repository and create a file called terraform.tfvars.
Inside this file, you'll include all the parameters for your deployment. You can select any resource_bucket and
analytics_bucket you want, as long as they don't exist already. Your analytics_bucket is where the final results
will live so that they can be analyzed using QuickSight. Any region is acceptable, but it must match your QuickSight
region if you want to create dashboards.

```
prefix="iwfm"
iwfm_model="<path-to-model>/c2vsimfg_version1.01.zip"
resource_bucket="iwfm-bucket-987342582"
analytics_bucket="iwfm-analytics-3523598"
region="us-east-2"
aws_access_key="your_access_key"
aws_secret_key="your_secret_key"
```

If you are doing multiple deployments, you can distinguish between them using prefix, but otherwise
just leave it as "iwfm". Make sure the iwfm_model is the one you intend to run. us-east-2 is Ohio,
but any region the support AWS ECS will work for the deployment. The resource bucket needs to be
globally unique, but it can be any string.

Now you are ready to deploy. You can initialize terraform (which will download the AWS provider),
and then apply the deployment.

```
terraform init
terraform apply
```

This will prompt you to agree with the proposed changes. After typing "yes" and pressing enter, it
will get to work deploying everything. This takes some time, but you can take a look at it in the
AWS console by visiting the "Elastic Container Service".

To access your deployment, select the iwfm-cluster in the "Elastic Container Service" dashboard.
Then select the iwfm-service. Then finally, select the one running task inside that service. The
info on the task will display its public IP address, which you can use to reach the deployment
from any browser using http://public.ip.address/files, where public.ip.address is the address for
that task.

If you visit "Cloud Watch" in the AWS console, you should see a dashboard created called
iwfm-dashboard, where you can monitor the performance and log output of the project.

### Cleaning Up

TODO: Explain how to isolate and preserve the analysis results using the process:

```
# list all resources
terraform state list

# remove that resource you don't want to destroy
# you can add more to be excluded if required
terraform state rm <resource_to_be_deleted>

# destroy the whole stack except above excluded resource(s)
terraform destroy
```

When you are done, and detached analytics results from your deployment, you can destroy the deployment.

```
terraform destroy
```

## Deploying PEST++ to AWS

Follow the same instructions for deploying the IWFM model, but use the directory terraform/pestpp-aws.

```
terraform destroy
```

## Deploying the Simulation to Azure

Note: Running on Azure is currently not supported, because the ephemeral space for containers is
only 15gb. The work here could be used as a starting point, but will fail due to the small size
permitted by Azure.

~~Terraform scripts are provided here to allow any Microsoft user to perform the same operation in
their Azure Cloud account. Because all the results are accessible via the browser, there is no
need to even deal with the Azure console, you can monitor the process from anywhere.~~

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
subscription you want to use. Make sure to run these commands from the terraform/iwfm-azure directory
located within where ever you checked out this GitHub repository.

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
