library(caTools)
library(dplyr)
library(tidyr)
library(stringr)
library(rpart)
library(rpart.plot)
library(jsonlite)
library(tidyjson)
library(rattle)
library(ggplot2)
library(tm)
library(SnowballC)
library(wordcloud)
library(cluster)

# N.B. not shown here: improing JSON data from Yelp and exporting as separate RDS files

businesses = readRDS("businesses.rds")
checkins = readRDS("checkins.rds")
tips = readRDS("tips.rds")
reviews = readRDS("reviews.rds")
users = readRDS("users.rds")
restaurants = businesses[grep("Restaurant", business$categories),]
saveRDS(restaurants, "restaurants.rds")
restaurantsToronto = subset(restaurants, city == "Toronto")
restaurantsVegas = subset(restaurants, city == "Las Vegas")
restaurants = readRDS("restaurants.rds")
restaurantsToronto = readRDS("restaurantsToronto.rds")
restaurantsVegas = readRDS("restaurantsVegas.rds")
restaurantsVegas$longitude[restaurantsVegas$business_id=='wLIkuQuEIcqQ94060rQUaQ'] = -115.17495
reviewsRestaurantsToronto = subset(reviews, business_id %in% restaurantsToronto$business_id)
reviewsRestaurantsVegas = subset(reviews, business_id %in% restaurantsVegas$business_id)
reviewsRestaurants = subset(reviews, business_id %in% restaurants$business_id)
saveRDS(reviewsRestaurantsToronto, "reviewsRestaurantsToronto.rds")
saveRDS(reviewsRestaurantsVegas, "reviewsRestaurantsVegas.rds")
saveRDS(reviewsRestaurants, "reviewsRestaurants.rds")
reviewsRestaurantsAll = readRDS("reviewsRestaurants.rds")
reviewsRestaurantsToronto = readRDS("reviewsRestaurantsToronto.rds")
reviewsRestaurantsVegas = readRDS("reviewsRestaurantsVegas.rds")

# Functions: ####

AddMetrics = function(x) {
	x$checkins = apply(x, 1, function(y) checkins$time[which(y$business_id == checkins$business_id)])
	rgx = "(?<=:)[0-9]+"
	lt = str_extract_all(x$checkins, rgx)
	x$checkins = NULL
	x['checkins'] = as.data.frame(unlist(lapply(lt, function(row) as.integer(sum(strtoi(unlist(row)))))))
	x$performance = (x$stars + x$is_open) * log(x$review_count+x$checkins)
	return(x)
}

ExpandCategories = function(x) {
	categories = unique(unlist(as.list(x$categories),recursive=F))
	categories = categories[categories != "Restaurants"]
	categories = tolower(str_replace_all(categories, " ", "_"))
	categories = paste("cat.", categories, sep="")
	x$categories = tolower(str_replace_all(x$categories, " ", "_"))
	for (category in categories){
		cat = strsplit(category, "\\.")
		x[category] = as.numeric(grepl(cat[[1]][2], x$categories))
		if (sum(x[category] == TRUE) < 0.01*nrow(x)) x[category] = NULL
	}
	x$categories = NULL
	return(x)
}

ExpandAttributes = function(x) {
	testat =  unique(unlist(as.list(x$attributes),recursive=F))
	for (attribute in testat){
		if(length(grep("\\{", attribute)) != 0){
			name = strsplit(strsplit(attribute, "\\}")[[1]][1], "\\:")[[1]][1]
			subvalues = strsplit(strsplit(strsplit(attribute, "\\}")[[1]][1], "\\{")[[1]][2], ",")[[1]]
			for (subvalue in subvalues){
				subvalue = gsub(' ', '', subvalue)
				subvalue = noquote(strsplit(subvalue, "\\:")[[1]][1])
				subvalue = gsub('^.|.$', '', subvalue)
				coltitle = paste("attr.", name, ".", noquote(subvalue), sep="")
				if(!(coltitle %in% colnames(x))){
					regexpat = paste("(?<=", subvalue, "\\':\\W)[A-Za-z0-9]+", sep="")
					newvec = str_extract(x$attributes, regexpat)
					if (!all(na.omit(newvec)[1] == na.omit(newvec))){
						x[coltitle] = newvec
					}
					regexdel = paste(attr, "\\':\\W[A-Za-z0-9]+", sep="")
					x$attributes = gsub(regexdel, "", x$attributes)
				}
			}
		} else {
			attr = strsplit(attribute, "\\:")[[1]][1]
			regexpat = paste("(?<=", attr, ":\\W)[A-Za-z0-9]+", sep="")
			coltitle = paste("attr.", attr, sep="")
			if(!(coltitle %in% colnames(x))){
				newvec = str_extract(x$attributes, regexpat)
				if (!all(na.omit(newvec)[1] == na.omit(newvec))){
					x[coltitle] = newvec
				}
				regexdel = paste(attr, ":\\W[A-Za-z0-9]+", sep="")
				x$attributes = gsub(regexdel, "", x$attributes)
			}
		}
	}
	x$attributes = NULL
	return(x)
}

RunLogistic = function(x) {
	#x$categories = NULL
	x$attributes = NULL
	x$categories = NULL
	x = subset(x, select=-c(type, hours, state, city, business_id, name, neighborhood, address, postal_code, longitude, latitude, performance))
	set.seed(100)
	split = sample.split(x, SplitRatio = 0.7)
	train = x[split == TRUE,]
	test = x[split == FALSE,]
	fit = glm(is_open ~ ., data=train, family="binomial")
	#pred = predict(fit, newdata=test, type="response")
	#acc = table(test$not.fully.paid, pred > 0.2)/nrow(test)
	print(summary(fit))
	return(fit)
}

RunCart = function(x, cex=NULL, cp=NULL, sub="") {
	x$categories = NULL
	x$attributes = NULL
	x = subset(x, select=-c(type, hours, state, city, business_id, name, neighborhood, address, postal_code, longitude, latitude, is_open, review_count, stars, checkins))
	set.seed(100)
	split = sample.split(x, SplitRatio = 0.7)
	train = x[split == TRUE,]
	test = x[split == FALSE,]
	tree = rpart(performance ~ ., data=train, method="anova", cp=cp)
	fancyRpartPlot(tree, sub=sub, cex=cex)
}

RunLocationClustering = function(x, n=10) {
	x = ExpandAttributes(x)
	xLatLong = x[c('latitude', 'longitude')]
	set.seed(100)
	km <- kmeans(xLatLong, iter.max=100, n)
	kmcluster = as.data.frame(km$cluster)
	x$cluster = kmcluster
	cs = seq(1,n)
	# compute averages for each cluster: (price/stars/reviews/checkins)
	c1 = x[x$cluster==1,]
	clusters = list()
	clusters[0] = x[x$cluster==1,]
	averages = data.frame(price=numeric(n), stars=numeric(n), checkins=numeric(n), reviews=numeric(n), performance=numeric(n), n=numeric(n))
	for (c in cs) {
		cluster = x[x$cluster==c,]
		averages$n[c] = nrow(cluster)
		averages$stars[c] = mean(cluster$stars)
		averages$checkins[c] = mean(cluster$checkins)
		averages$performance[c] = mean(cluster$performance)
		averages$reviews[c] = mean(cluster$review_count)
		averages$price[c] = mean(as.numeric(cluster$attr.RestaurantsPriceRange2), na.rm=TRUE)
	}
	plot(xLatLong,col=km$cluster)
	print(averages)
	return (averages)
}

PlotWordCloud = function(x, color="black") {
	rc <- Corpus(VectorSource(x$text))
	rc <- tm_map(rc, removePunctuation)
	rc <- tm_map(rc, tolower)
	rc <- tm_map(rc, removeWords, c('the', 'this', stopwords('english')))
	wordcloud(rc, max.words=100, random.order=FALSE, colors=color)
}

CategoryClusters = function(x, n=9) {
	y <- ExpandCategories(x)
	x <- ExpandAttributes(x)
	y <- subset(y, select=-c(type, hours, state, city, business_id, name, neighborhood, address, postal_code, longitude, latitude, stars, review_count, is_open, checkins, performance, attributes))
	#y <- as.data.frame(unclass(y))
	d <- dist(y)
	mod.hclust <- hclust(d, method="ward.D2")
	plot(mod.hclust, labels=F, xlab=NA, ylab="Dissimilarity",sub=NA, main=NA)
	cx <- data.frame(k = seq_along(mod.hclust$height),dissimilarity = rev(mod.hclust$height))
	ggplot(cx, aes(x=k, y=dissimilarity)) + geom_line()+xlab("Number of Clusters") +ylab("Dissimilarity") + xlim(0,50)
	assignments <- cutree(mod.hclust, n)
	y$cluster = assignments
	clustermeans = subset(data.frame(y[1,]), select=-c(cluster))
	clustercats = list(n)
	clusterscores = data.frame(cats=character(n), price=numeric(n), stars=numeric(n), checkins=numeric(n), reviews=numeric(n), performance=numeric(n), n=numeric(n), stringsAsFactors=FALSE)
	cs = seq(1,n)
	for (c in cs) {
		cluster = subset(y[y$cluster==c,], select=-c(cluster))
		clustermeans[c,] = colMeans(cluster)
		clustercats[c] = list(rownames(as.data.frame(which(apply(clustermeans[c,], 2, function(i) i>0.1)))))
		clusterx = x[rownames(cluster),]
		clusterscores$n[c] = nrow(cluster)
		clusterscores$stars[c] = mean(clusterx$stars)
		clusterscores$checkins[c] = mean(clusterx$checkins)
		clusterscores$performance[c] = mean(clusterx$performance)
		clusterscores$reviews[c] = mean(clusterx$review_count)
		clusterscores$price[c] = mean(as.numeric(clusterx$attr.RestaurantsPriceRange2), na.rm=TRUE)
		clusterscores$cats[c] =  paste(unlist(clustercats[c]), collapse=', ')[1]
	}
	return (clusterscores)
}

# Preprocess data: ####

dfList = lapply(list(restaurantsToronto, restaurantsVegas, restaurants), AddMetrics)
dfListCategories = lapply(dfList, ExpandCategories)
dfListAttributes = lapply(dfList, ExpandAttributes)
restaurantsToronto = dfList[[1]]
restaurantsVegas = dfList[[2]]
restaurantsAll = dfList[[3]]

# Which types of restaurant are most likely to stay open? (logistic regression on is_open using categories) ####

restaurantsTorontoLogistic = dfListCategories[[1]]
restaurantsVegasLogistic = dfListCategories[[2]]
restaurantsAllLogistic = dfListCategories[[3]]
RunLogistic(restaurantsTorontoLogistic)
RunLogistic(restaurantsVegasLogistic)
RunLogistic(restaurantsAllLogistic)

# Which restaurant attributes lead to the best performance? (CART tree on performance using attributes and categories) ####

restaurantsTorontoCategories = dfListCategories[[1]]
restaurantsTorontoAttributes = dfListAttributes[[1]]
restaurantsVegasCategories = dfListCategories[[2]]
restaurantsVegasAttributes = dfListAttributes[[2]]
restaurantsAllCategories = dfListCategories[[3]]
restaurantsAllAttributes = dfListAttributes[[3]]
RunCart(restaurantsTorontoCategories, cex=0.7, cp=0.0045, sub="Performance score of restaurants in Toronto by category")
RunCart(restaurantsTorontoAttributes, cex=0.7, cp=0.005, sub="Performance score of restaurants in Toronto by attribute")
RunCart(restaurantsVegasCategories, cex=0.5, cp=0.005, sub="Performance score of restaurants in Las Vegas by category")
RunCart(restaurantsVegasAttributes, cex=0.6, cp=0.007, sub="Performance score of restaurants in Las Vegas by attribute")
RunCart(restaurantsAllCategories, cex=0.6, cp=0.0035, sub="Performance score of all restaurants by category")
RunCart(restaurantsAllAttributes, cex=0.6, cp=0.0065, sub="Performance score of all restaurants by attribute")

# What terms appear most often in five-star reviews of restaurants? (wordcloud using tm, wordcloud, snowballc) ####

reviewsRestaurantsTorontoFiveStars = reviewsRestaurantsToronto[reviewsRestaurantsToronto$stars==5,]
reviewsRestaurantsTorontoOneStars = reviewsRestaurantsToronto[reviewsRestaurantsToronto$stars==1,]
PlotWordCloud(reviewsRestaurantsTorontoOneStars, color="red")
PlotWordCloud(reviewsRestaurantsTorontoFiveStars, color="green")

reviewsRestaurantsVegasFiveStars = reviewsRestaurantsVegas[reviewsRestaurantsVegas$stars==5,]
reviewsRestaurantsVegasOneStars = reviewsRestaurantsVegas[reviewsRestaurantsVegas$stars==1,]
PlotWordCloud(reviewsRestaurantsVegasOneStars, color="red")
PlotWordCloud(reviewsRestaurantsVegasFiveStars, color="green")

reviewsRestaurantsAllFiveStars = reviewsRestaurantsAll[reviewsRestaurantsAll$stars==5,]
reviewsRestaurantsAllOneStars = reviewsRestaurantsAll[reviewsRestaurantsAll$stars==1,]
PlotWordCloud(reviewsRestaurantsAllOneStars, color="red")
PlotWordCloud(reviewsRestaurantsAllFiveStars, color="green")

# What neighbourhood has the highest performing restaurants in each city? (cluster by lat/long) ####

torontoLocationClusters <- RunLocationClustering(restaurantsToronto, n=8)
vegasLocationClusters <- RunLocationClustering(restaurantsVegas, n=10)

# What are the most significant types of restaurants in each city and overall? (clustering on attributes + categories) ####

torontoClusters <- CategoryClusters(restaurantsToronto, n=9)
vegasClusters <- CategoryClusters(restaurantsVegas, n=9)
allClusters <- CategoryClusters(restaurantsAll, n=12)
View(torontoClusters)
View(vegasClusters)
View(allClusters)

