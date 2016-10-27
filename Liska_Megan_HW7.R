#HW 7
#Group C

require(ggplot2) #we will need the ggplot library for the graphs for 3 and 4 
#1 
freqtable <- function(x){
  #freqtable loops through a dataframe and creates a frequency table for every
  #factor column. 
  #Parameters:
  #x - a dataframe
  
  #Returns:
  #Frequency table 
  factorcols <- x[sapply(x, is.factor)] #uses sapply to grab the columns of 
  #dataframe x which are factor columns (finds factor columns using is.factor)
  print(summary(factorcols))
}
freqtable(diamonds) #test case 

#2a
colsummary <- function(x){
  #colsummary takes a dataframe as its input, and yields the summary of the
  #numeric columns
  #Parameters:
  #x - a dataframe
  
  #Returns:
  #summary of the columns  
  
  numeric <- sapply(x,is.numeric) #sapply applies is.numeric to each column
  #in the dataframe to extract the numeric columns
  print(sapply(x[,numeric], summary)) #sapply then applies summary to the numeric
  #columns of the dataframe and prints them 
}

#2b

rsquare <- function(x){
  #rsquare takes any dataframe as a parameter and returns a dataframe that contains
  #each pair of column names in the first column as a single string seperated
  #by a "-" and their corresponding () in the second column
  
  #Parameters:
  #x - a dataframe
  
  #Returns:
  #a new dataframe 
  
  a <- x[sapply(x, is.numeric)]
  if(ncol(a) >= 2) {
    b <- combn(colnames(a), 2) #finds all combinations of the name pairs
    
    pairs <- paste(b[1,], b[2, ], sep = "-") #makes sure that the column names
    #are seperated by - using paste function to paste the columns
    
    c <- cor(a, method = "pearson")#finds the pearson correlation using the cor 
    #function and creates of matrix of the values
    
    correlation <- c[which(lower.tri(c))] #gets the correlation values of the lower 
    #triangular matrix since those match the column pairs 
    
    newdf <- data.frame(pairs, correlation) #create a new data frame with our pairs 
    return(newdf)
    
  }
  else  #print this message if we can't find Pearson correlation
    print("Pearson Correlation cannot be computted because there are not enough numeric columns")

}

#2c
#Use the corCoef function I created for HW 5
corCoef <- function(x){
  #corCoef takes any dataframe as a parameter and returns a dataframe that 
  #contains each pair of column names in the first column as a single string
  #seperated by a "-" and their corresponding Pearson correlation coefficient
  #in the second column.
  
  #Parameters:
  #x - a dataframe
  
  #Returns:
  #dataframe
  a <- x[sapply(x, is.numeric)] #creates a variable that only has the numeric 
  #columns
  
  #we will use an if-else statement for this problem
  #we need at least 2 columns to find the Pearson correlation
  if(ncol(a) >= 2) {
    b <- combn(colnames(a), 2) #finds all combinations of the name pairs
    
    pairs <- paste(b[1,], b[2, ], sep = "-") #makes sure that the column names
    #are seperated by - using paste function to paste the columns
    
    c <- cor(a, method = "pearson")#finds the pearson correlation using the cor 
    #function and creates of matrix of the values
    
    correlation <- c[which(lower.tri(c))] #gets the correclation values of the lower 
    #triangular matrix since those match the column pairs 
    
    newdf <- data.frame(pairs, correlation) #create a new data frame with our pairs 
    return(newdf)
    
  }
  else  #print this message if we can't find Pearson correlation
    print("Pearson Correlation cannot be computted because there are not enough numeric columns")
}
#We don't want all these values, we only want the ones that are greater than, so add a new function

abs_pearson <- function(dataset, threshold){
  #abs_pearson function takes a dataframe of with pearson correlations of each 2 variables
  #and extract the values that are greater than a threshold based on user input
  
  #Parameters: 
  #dataframe
  
  #Results: 
  #the combination of column names and their Pearson coefficients values 
  #that are greater than the threshold
  row_index <- which(abs(dataset[,2]) > threshold)      #determine which column is greater than threshold
  return(dataset[row_index, ])                           #return a new dataframe with the updated coefficients
}


#3
#I got the multiplot function from the R-cookbook and rewrote it here.
#I use it for question 3 to combine subplots into a grid and print the grid
#Reference: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}




numeric_plot <- function(data, plot_switch, binVec) {
  #The numeric_plot function executes in the following directions: 
  
  #If the plot switch parameter is “on” or “grid”, numeric_plot plots a pair of blue histograms 
  #with a vertical red line at the mean (one using counts and the other density) for 
  #every numerical variable at each number of bins integer specified in the bin vector parameter. 
  
  #If the plot switch is set to “grid”, then numeric_plot prints a grid for each count-bin 
  #combination and a separate grid for each density-bin size combination.
  
  #Parameters: 
  #data- dataframe, plot_switch- string, binVec - vector
  
  #Returns: 
  #grid plots with count and desnity histograms
  
  num <- sapply(data, is.numeric) #first we get the numeric columns of the dataset
  data <- data[,num]              #and put them in a new variable
  for(name in colnames(data)) {   #We will loop through the columns of the dataset and use a series
                                  #of if-else statements to deal with the cases in the question
    
    if(plot_switch == "on"){      #1st case when switch is "on"
      grid.newpage()          
      m <- lapply(data[name], mean)       #find the mean of the column so we can plot our vertical line
      plot1 <- ggplot(data, aes_string(name)) + geom_histogram(fill="blue") + geom_vline(xintercept = m[[1]], colour="red") 
      plot2 <- ggplot(data, aes_string(name)) + geom_histogram(aes(y= ..density..), fill="blue") + geom_vline(xintercept = m[[1]], colour="red")
      #creates two histograms, one for blank and one for blank
      #geom_vline adds our vertical line
      multiplot(plot1, plot2, cols = 1)
      #use multiplot to plot the histograms on our grid 
      

    }
    
    if(plot_switch == "grid"){    #2nd case when switch is "grid"
      count_plots <- list()       #Create list to store histogram subplots of each bin size
      density_plots <- list()           #Create list to store the density histograms subplots of each bin size
      if(missing(binVec)){              #case when the vector is null and prints histogram using the default bins 30
        print(ggplot(data, aes_string(name), color = "blue") + geom_histogram(fill="blue")+ labs(title= "default bins"))
        print(ggplot(data, aes_string(name), color = "blue") + geom_histogram(aes(y= ..density..), fill="blue")+ labs(title= "default bins"))
        #prints out two historgrams using ggplot 
      }else{                            
        for(i in 1:length(binVec)) {    #loop through each bin size in binVec and create a subplot
          k <- ggplot(data, aes_string(name), color = "blue") + geom_histogram(fill="blue", bins = binVec[i])+ labs(title= paste(binVec[i], "bins"))
          count_plots[[i]] <- k           #Puts subplots in a list 
        }
        multiplot(plotlist = count_plots, cols = 2)  #uses multiplot to plot our graphs   
        #we repeat the process expcept for the second histogram 
        for(i in 1:length(binVec)) {   
          k <- ggplot(data, aes_string(name), color = "blue") + geom_histogram(aes(y= ..density..), fill="blue", bins = binVec[i])+ labs(title= paste(binVec[i], "bins"))
          density_plots[[i]] <- k       
        }
        multiplot(plotlist = density_plots, cols = 2)
        
      }
    }
  }
}


#4
cata_binary_plot <-function(data, plot_switch){
  #cata_binary_plot plots a gray bar graph for every categorical and binary variable.
  #when the plot switch parameter is “on” or “grid"
  
  #Parameters: 
  #data - a dataframe, plot_switch- a string
  
  #Returns: bar graphs
  
  cata_binary <- sapply(data, function(x) (is.factor(x) || is.logical(x)) || is.binary(x))    
  cata_binary_data <- data[cata_binary]     #extract binary and categorical columns 
  
  if(plot_switch == "on" || plot_switch == "grid") {      #if plot switch is on or grid we want our graphs
    for(name in colnames(cata_binary_data)) {         #loop through the sorted dataframe and plot bar graphs for each column
      j <- ggplot(cata_binary_data, aes_string(name), color = "grey") + geom_bar(fill="grey")
      #use ggplot to make our graph and print it 
      print(j)
    }
  }
}

#Now finally we will take all the functions that we have made and combine them into our explore 
#function as was directed in the homework 
explore <-function(a,b,c){
  #explore takes four parameters (name them)
  #Parameters:
  
  #Returns:
  
}