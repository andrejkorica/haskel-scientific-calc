import Graphics.Gloss

main :: IO ()
main = do   
    -- Display the combined pictures
    let combinedPictures = chessboard 
    display 
        (InWindow "Chessboard" (450, 450) (500, 500)) -- Window properties
        black                                    -- Background color 
        combinedPictures                         -- Picture to display

-- Define the chessboard pattern
chessboard :: Picture
chessboard = Pictures [drawSquare x y | x <- [1..8], y <- [1..8]]

-- Function to draw a single square of the chessboard
drawSquare :: Int -> Int -> Picture
drawSquare x y = Translate (fromIntegral x * 50 - 225) (fromIntegral y * 50 - 225) $ 
                 Color (if even (x + y) then darkBrown else lightBrown) $
                 rectangleSolid 50 50

-- Define custom colors
darkBrown :: Color
darkBrown = makeColorI 139 69 19 255  -- Saddle Brown
 
lightBrown :: Color
lightBrown = makeColorI 244 164 96 255 -- Sandy Brown