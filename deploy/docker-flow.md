# Build, Run, and Publish with Docker



## Requirements



Before you begin, make sure you have:



- [Docker Desktop](https://www.docker.com/products/docker-desktop/) installed and running.

- A [Docker Hub](https://hub.docker.com/) account.

- A `Dockerfile` in your project directory.



## Step 1: Build the Docker Image



Open a terminal in your project directory and build the Docker image.



**General command:**



```bash

docker build -t {docker_user_name}/{docker_image_name}:{version} .

```



**Example:**



```bash

docker build -t willemboone/ph1-rshiny:v1.0.0 .

```



This command builds the image using the `Dockerfile` in the current directory and assigns it a name and version tag.



### Test the Docker Image



Run the image locally to verify that the Shiny application works correctly.



**General command:**



```bash

docker run --rm --name ph1-rshiny -p 3838:3838 {docker_user_name}/{docker_image_name}:{version}

```



**Example:**



```bash

docker run --rm --name ph1-rshiny -p 3838:3838 willemboone/ph1-rshiny:v1.1.0

```



Open your browser and navigate to:



**http://localhost:3838**



Verify that the Shiny application loads and functions as expected.



The `-p 3838:3838` option maps port `3838` on your host machine to port `3838` inside the container. The `--name` option gives the container a stable name, and `--rm` automatically removes it when it stops.

Do not use `-P` or a port mapping such as `-p 3838` because Docker may assign a random host port. Always use the explicit `-p 3838:3838` mapping above.



## Step 2: Publish the Docker Image to Docker Hub



### Log in to Docker Hub



Run the following command:



```bash

docker login

```



Follow the prompts to authenticate with your Docker Hub account.



### Push the Image



**General command:**



```bash

docker push {docker_user_name}/{docker_image_name}:{version}

```



**Example:**



```bash

docker push willemboone/ph1-rshiny:v1.0.0

```



This uploads the image to Docker Hub.



**Important:** The Docker Hub repository `{docker_user_name}/{docker_image_name}` must exist before you push the image. For this example, create the `ph1-rshiny` repository under your Docker Hub account if it does not already exist.


