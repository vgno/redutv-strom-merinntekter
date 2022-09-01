LAST_COMMIT_ID:=$(shell git rev-parse --short HEAD)
DOCKER_IMAGE_NAME=vgredutv/strom-merinntekter

deploy: docker-build docker-tag docker-push

docker-build:
	docker build --platform linux/amd64 -t $(DOCKER_IMAGE_NAME):$(LAST_COMMIT_ID) .

docker-tag: check-env
	docker tag $(DOCKER_IMAGE_NAME):$(LAST_COMMIT_ID) $(DOCKER_REGISTRY)/$(DOCKER_IMAGE_NAME):$(LAST_COMMIT_ID)
	docker tag $(DOCKER_IMAGE_NAME):$(LAST_COMMIT_ID) $(DOCKER_REGISTRY)/$(DOCKER_IMAGE_NAME):latest

docker-push: check-env
	docker push $(DOCKER_REGISTRY)/$(DOCKER_IMAGE_NAME):$(LAST_COMMIT_ID)
	docker push $(DOCKER_REGISTRY)/$(DOCKER_IMAGE_NAME):latest

docker-run: docker-build
	docker run --rm \
    -v $(PWD)/outputs:/app/outputs \
    -e API_PRICES_DAILY="$(API_PRICES_DAILY)" \
    -e API_PROD_DAILY="$(API_PROD_DAILY)" \
    -e API_PROD_HOURLY="$(API_PROD_HOURLY)" \
    -e API_PRICES_HOURLY="$(API_PRICES_HOURLY)" \
    $(DOCKER_IMAGE_NAME):$(LAST_COMMIT_ID)


check-env:
ifndef DOCKER_REGISTRY
	$(error DOCKER_REGISTRY must be set)
endif
