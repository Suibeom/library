module Lib
    (
    ) where

-- Just a bit of a Rust reflex here
data Result = Ok | Error String

-- Getting real dates and times needs IO, so we're going with this for now.
data DummyTime = DummyTime Int

-- Gonna prefer record syntax here because I get some free self-description.
-- ISBN should be validated before these are allowed to be constructed! But for now it's a string
data Book = Book {title :: String, author :: String, isbn :: String, totalCopies :: Int}

data BookAvailability = Unavailable | Available {copies :: Int}

-- So I'm diverging the internal and external models of the data here a little bit.
-- I'm not gonna attach the list of books _directly_ to the Patron object.
-- Instead I'll use a 'database' of loans to associate them.
-- This will allow me to leave the Patron and Book objects immutable.
data Patron = Patron {name :: String, cardNumber :: String}

data Loan = Loan {borrowingPatron :: Patron, loanedBook :: Book, dueDate :: DummyTime}


-- This is gonna stand in for the database backing the library in this little abstraction.
data LibraryDB = LibraryDB {patrons :: [Patron], books :: [Book], loans :: [Loan]}

addBook :: LibraryDB -> Book -> LibraryDB
addBook (LibraryDB patrons books loans) book@Book {totalCopies = newCopies} =
    case filter (\x-> isbn book == isbn x) books of
        -- Book doesn't exist in the library yet, just add its
        [] -> LibraryDB patrons (book:books) loans
        -- Book is in the library already, just update the copies
        [Book {title = title, author = author, isbn = iSBN, totalCopies = currentTotal}] ->
            let otherBooks = filter (\x-> iSBN /= isbn x) books
                updatedBook = Book {title = title, author = author, isbn = iSBN, totalCopies = currentTotal + newCopies}
            in LibraryDB patrons (updatedBook:otherBooks) loans
        _ -> error "Shouldn't be more than one book entry with the same isbn!"


removeBook :: LibraryDB -> Book -> LibraryDB
removeBook (LibraryDB patrons books loans) Book {isbn = iSBN} =
    let filteredBooks = filter (\x -> iSBN /= isbn x) books
        -- Taking a book out of the system should remove any extant loans too
        filteredLoans = filter (\x -> iSBN /= isbn (loanedBook x)) loans
        in LibraryDB patrons filteredBooks filteredLoans

-- This is a dummy function to represent some matching logic. For now it'll be just a stupid string match.
searchMatch :: String -> String -> Bool
searchMatch search candidate = search == candidate

findBooksByAuthor :: LibraryDB -> String -> [Book]
findBooksByAuthor (LibraryDB _ books _) author' = filter (\x -> searchMatch author' (author x)) books
findBooksByTitle :: LibraryDB -> String -> [Book]
findBooksByTitle (LibraryDB _ books _) title' = filter (\x -> searchMatch title' (title x)) books
-- We should only have one book with an ISBN
findBookByISBN :: LibraryDB -> String -> Maybe Book
findBookByISBN (LibraryDB _ books _) isbn' =
    case filter (\x -> isbn' == isbn x) books of
        [] -> Nothing
        [book] -> Just book
        _ -> error  "Shouldn't be more than one book entry with the same isbn!"

countLoans :: LibraryDB -> Book -> Int
countLoans (LibraryDB _ _ loans) book = length (filter (\x -> isbn (loanedBook x) == isbn book) loans)


checkAvailability :: LibraryDB -> Book -> BookAvailability
checkAvailability db book =
    let availableCopies = (totalCopies book - countLoans db book)
    in if availableCopies > 0 then Available availableCopies
    else Unavailable

-- Dummy logic for deciding a new due date! Right now just returns existing time.
newDueDate :: DummyTime -> DummyTime
newDueDate (DummyTime time) = DummyTime (time + 7)

borrowBook :: LibraryDB -> Patron -> Book -> (LibraryDB, Result)
borrowBook db@(LibraryDB patrons books loans) patron book =
    case checkAvailability db book of
        Unavailable -> (db, Error "No copies available")
        Available _ ->
            let loan = Loan {borrowingPatron = patron, loanedBook = book, dueDate = DummyTime 0}
            in
                (LibraryDB patrons books (loan:loans), Ok)


-- This can't really fail, but it seems like the kind of thing that should have a Result return type anyways, if something weird happens!
returnBook :: LibraryDB -> Patron -> Book -> (LibraryDB, Result)
returnBook (LibraryDB patrons books loans) patron book =
    let remainingLoans = filter (\x ->( isbn (loanedBook x) /= isbn book) || (cardNumber (borrowingPatron x) /= cardNumber patron)) loans
    in
        (LibraryDB patrons books remainingLoans, Ok)