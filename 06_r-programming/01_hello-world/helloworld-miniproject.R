#Q5.1 & 5.2

#For this exercise, we will be using the tidyverse library.
#In the case that you do not have the library installed, run the code written below

install.packages("tidyverse")

#Alternatively, if you already have the library installed, you may run the following code

library(tidyverse)

#Finally, using only the GUI, you can install the requisite libraries and run code lines 6. To install a library using only the GUI, click on the "Tools" option, and select "Install Packages". Then, type the name of the library/package that you wish to install.

#Q6.1 & 6.2

#For the following part, I use the "Hello World" string to incorporate arithmetic, comparison, and logical operators within a data transformation pipeline

TextQuery <- "Hello World"
#To obtain the sum of letters in a given string, the input string is split into words, converted to a vector, and used to derive the length of each word. Finally, "sum()" provides the total number of letters.
SumOfLetters <- str_split(TextQuery, " ")%>%
  unlist()%>%
  map_int(str_length)%>%
  sum()

print(SumOfLetters)

#For a comparison operator, another string input is introduced in the form of "Hi World". The objective here was to compare the number of letters in both these string inputs.
text1 <- "Hello World"
text2 <- "Hi World"

#The total number of letters in the strings were derived (as in ln 20-23)
sum_letters <- function(text) {
  str_split(text, " ") %>%
    unlist() %>%
    map_int(str_length) %>%
    sum()
}

#The "greater than" comparison operator was used to test whether the string "Hi World" has more letters than "Hello World".
letters_text1 <- sum_letters(text1)
letters_text2 <- sum_letters(text2)

#Use a comparison operator (e.g., greater than)
comparison <- letters_text2 > letters_text1

#Print the results
cat("Total letters in '", text1, "':", letters_text1, "\n")
cat("Total letters in '", text2, "':", letters_text2, "\n")
cat("Is '", text2, "' greater in total letters than '", text1, "'?", comparison, "\n")



#The idea in incorporating a logical operator was to test two things - first if the total number of letters in "Hi World" are less than or equal to that in "Hello World", and second, if the total number of letters in "Hi World" are more than 5. If both these conditions are to be met, the final result is a combination of these two outputs, and is conditional on both these conditions being met.
text1 <- "Hello World"
text2 <- "Hi World"

#The total number of letters in the strings were derived (as in ln 20-23)
sum_letters <- function(text) {
  str_split(text, " ") %>%
    unlist() %>%
    map_int(str_length) %>%
    sum()
}

#Calculate the total letters in each string
letters_text1 <- sum_letters(text1)
letters_text2 <- sum_letters(text2)

#Introducing the two conditions
comparison1 <- letters_text2 <= letters_text1  
comparison2 <- letters_text2 > 5            

#Combined final result of both conditions. A positive final result is contingent on both results being true.
result <- comparison1 & comparison2          

# Print the results
cat("Total letters in '", text1, "':", letters_text1, "\n")
cat("Total letters in '", text2, "':", letters_text2, "\n")
cat("Condition 1 (<=):", comparison1, "\n")
cat("Condition 2 (> 5):", comparison2, "\n")
cat("Final result (AND):", result, "\n")


#Q7.1, 7.2, & 7.3


#The idea here was to define a function with the total number of letters, and one optional argument as a reference to perform an operation on the strings.
calculate_letters <- function(text, reference = NULL) {
  #The total number of letters in the strings were derived (as in ln 20-23)
  total_letters <- text %>%
    str_split(" ") %>%
    unlist() %>%
    map_int(str_length) %>%
    sum()
  
  #We introduce a condition where the final result must be a comparison IF we provide a reference number.
  if (!is.null(reference)) {
    comparison <- total_letters > reference
    message <- paste("Total letters in '", text, "' (", total_letters,
                     ") is greater than reference (", reference, "): ", comparison, sep = "")
    return(message)
  }
  
  #However, if NO reference number is provided, it outputs a statement with just the total letter count.
  return(paste("Total letters in '", text, "': ", total_letters, sep = ""))
}

#We now test both conditions, first with no reference provided and then with a reference for two different string inputs.
result1 <- calculate_letters("Hello World")
print(result1)


result2 <- calculate_letters("Hi World", reference = 8)
print(result2)

#Q8.1 & 8.2

#For this question, we parse through a vector of strings that we have been using so far, doing the same letters per string calculation, and then using a conditional statement to classify the strings based on the total number of letters in them.
texts <- c("Hello World", "Hi World", "My name is Mansha")

#Defining a function to process each string
process_strings <- function(texts, threshold = 10) {
  results <- map(texts, function(text) {
    #The total number of letters in the strings were derived (as in ln 20-23)
    total_letters <- text %>%
      str_split(" ") %>%
      unlist() %>%
      map_int(str_length) %>%
      sum()
    
    #Here, we set the conditions to classify the strings based on their total letter count. The threshold value is introduced (and can be tested for different values as desired) in ln 154)
    classification <- if (total_letters > threshold) {
      "Above Threshold"
    } else if (total_letters == threshold) {
      "At Threshold"
    } else {
      "Below Threshold"
    }
    
    #The result is output in a table with the string, total letters, and the category they are classified into
    tibble(
      Text = text,
      TotalLetters = total_letters,
      Classification = classification
    )
  })
  
  #Combine results into a single data frame
  bind_rows(results)
}

#Test the function with a threshold of 10
Categories <- process_strings(texts, threshold = 10)
print(Categories)