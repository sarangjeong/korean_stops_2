# Korean-stop-contrast
This is my project on Korean stop contrast and the relation between production and perception under a sound change.

docker run --pull=missing --rm -ti \
  --name rstudio-dev \
  -p 127.0.0.1:8787:8787 \
  -e DISABLE_AUTH=true \
  -v "C:\Users\hanpe\github\korean-stops:/home/rstudio/project" \
  --log-driver json-file \
  --log-opt max-size=10m \
  --log-opt max-file=3 \
  rocker/rstudio:latest


docker run --pull=missing --rm -ti `
  --name rstudio-dev `
  -p 127.0.0.1:8787:8787 `
  -e DISABLE_AUTH=true `
  -v C:/Users/hanpe/github/korean-stops:/home/rstudio/project `
  --log-driver json-file `
  --log-opt max-size=10m `
  --log-opt max-file=3 `
  rocker/rstudio:latest

## Running RStudio in Docker

To run the `rocker/rstudio:4.3.0` container, use the following command:

```bash
docker run -d \
  -p 8888:8787 \
  --rm \
  -e DISABLE_AUTH=true \
  --log-driver=none \
  -v "$(pwd):/home/rstudio/project" \
  rocker/rstudio:4.3.0
```

### Explanation:
- `-d`: Run the container in detached mode.
- `-p 8888:8787`: Map port 8888 on your host to port 8787 in the container.
- `--rm`: Automatically remove the container when it stops, ensuring no logs or data persist.
- `-e DISABLE_AUTH=true`: Disable authentication, so no password is required to access RStudio.
- `--log-driver=none`: Disable logging to prevent log files from consuming disk space.
- `-v "$(pwd):/home/rstudio/project"`: Mount the current project directory to `/home/rstudio/project` in the container.
- `rocker/rstudio:4.3.0`: The Docker image to use, with R version 4.3.0.
