const dictionary = require("words-pt");
const fs = require('fs')

dictionary.init({removeNames: true}, err => {
    if(err)
        return 
  
    const wordArray = dictionary.getArray();
    const fiveLetterWords = wordArray.filter(word => word.length === 5);
    
    fiveLetterWords.forEach(word => {
        fs.appendFile('words_output.pl', `palavra(${word}).\n`, 'utf8', (err) => {
            if (err) console.log("error appending")
        })
    });
    
    console.log(fiveLetterWords);
})
