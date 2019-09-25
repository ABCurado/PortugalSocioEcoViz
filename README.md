# Results of the 2015 Election in relation to socio-economic factors of Portuguese municipalities visualized

Visualization created in R using the shiny framework to better understand Portuguese municipalities Socio economic factors and voting behaviors

### Build docker image 

docker build -t municipalities_shiny_app .

### Run image

docker run -d -p 8080:80 --name shinyApp municipalities_shiny_app

### Access visualization

Check localhost:8080 in the browser
