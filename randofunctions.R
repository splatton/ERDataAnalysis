##This first function will anonymize any dataset by creating n^3 different silly names and then making a vector with that many names. The input will allow one to choose the size of the vector.

anonymizer <- function(n) {
  library(stringr)
  library(dplyr)
  first_name <- c('Mortimer', 'Geraldine', 'Frances', 'Stupendo', 'Bertha', 'Rhonda', 'Probost', 'Fleep', 'Donald Trump', 'Larry', 'Karen', 'Blippy', 'Cocoa', 'Throckmorton', 'Jo', 'Quincy', 'Phlebas', 'Firona', 'Graciella', 'Juniper', 'Ennis', 'Carla', 'Rosie', 'George', 'Funky', 'Punky', 'Sneezy', 'Chunk', 'Pulaski', 'Jose')
  middle_name <- c('McFunky', 'Scrooge', 'McLovin', 'Jameson', 'McBeer', 'Pungent', 'McRib', 'Barflegs', 'McPoop', 'Fart', 'McWeiner', 'Tobias', 'McMonkey', 'Danger', 'McDanger', 'Mixed Nuts', 'McNuts', 'Pants', 'McLoser', 'Bologna', 'McVomit', 'Pepperoni', 'McSquirrel', 'Queso', 'McViscous', 'Savory', 'McJohnson', 'Jiminy', 'McSprocket', 'Laser')
  last_name <- c('Christmas', 'July', 'Xavier', 'Enchilada', 'Pizzaface', 'Blueberry', 'Waco', 'Dallas', 'Smith', 'Jones', 'Lasgna', 'Benavides', 'McLeary', 'Williams', 'Johnson', 'Blungleton', 'Pimpleton', 'Fartworth', 'Trump', 'Thumbington', 'IV', 'Junior', 'Richards', 'McRibbington', 'Queasingwirth', 'Pinskoski')
  name_vector <- vector()
  for (i in 1:length(first_name)) {
    for (j in 1: length(middle_name)) {
      for (k in 1:length(last_name)) {
        temp_name <- str_c(first_name[i], middle_name[j], last_name[k], sep = ' ')
        name_vector <- c(name_vector, temp_name)
      }
    }
  }
  output_vector <- sample(name_vector, size = n, replace = FALSE)
  return(output_vector)
}