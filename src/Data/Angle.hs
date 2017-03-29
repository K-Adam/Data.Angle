
module Data.Angle where

import Data.Fixed -- mod'

newtype Angle a = Radians { angleValueRadians :: a } deriving (Eq, Show)


-- Creating Angle from a value


-- | Create an Angle with the given degrees
angleFromDegrees :: (Integral d, Floating r) => d -> Angle r
angleFromDegrees x = Radians $ (realToFrac x) * pi/180

-- | Create an Angle with the given turns
angleFromTurns :: (Real t, Floating r) => t -> Angle r
angleFromTurns x = Radians $ (realToFrac x) * pi*2

-- | Create an Angle with the given radians
angleFromRadians :: (Floating r) => r -> Angle r
angleFromRadians = Radians


-- Get the value from angle


-- | Get degrees from an angle
angleValueDegrees :: (Floating r, RealFrac r, Integral d) => Angle r -> d
angleValueDegrees (Radians x) = round $ x / pi * 180.0

-- | Get turns from an angle
angleValueTurns :: (Floating r) => Angle r -> r
angleValueTurns (Radians x) = x / (pi*2)


-- Functor and Applicative instance


instance Functor Angle where
    fmap f (Radians x) = Radians (f x)

instance Applicative Angle where
    pure = Radians       
    Radians f <*> r = fmap f r


-- Basic functions


-- | Adding two angles
addAngle :: (Floating a) => Angle a -> Angle a -> Angle a
addAngle r1 r2 = (+) <$> r1 <*> r2

-- | Normalize Angle: transforming back to (0-2pi)
normAngle :: (Floating a, Real a) => Angle a -> Angle a
normAngle (Radians r) = Radians $ mod' r (pi*2)

-- | Add two angles and normalize the result
addAngleNorm :: (Floating a, Real a) => Angle a -> Angle a -> Angle a
addAngleNorm a b = normAngle $ addAngle a b

-- | Distance between two angles
distAngle :: (Floating a, Real a) => Angle a -> Angle a -> Angle a
distAngle (Radians r1) (Radians r2) = Radians $ if (a' < b') then a' else b'
    where
        a' = mod' (r1-r2) (pi*2)
        b' = mod' (r2-r1) (pi*2)

-- | Flip angle
flipAngle :: (Floating a) => Angle a -> Angle a
flipAngle = fmap negate

-- | Flip angle and normalize the result
flipAngleNorm :: (Floating a, Real a) => Angle a -> Angle a
flipAngleNorm = normAngle . flipAngle

-- | Add degrees to angle
addAngleDegrees :: (Floating r, Integral d) => Angle r -> d -> Angle r
addAngleDegrees ang deg = addAngle ang $ angleFromDegrees deg

-- | Add radians to angle
addAngleRadians :: (Floating r) => Angle r -> r -> Angle r
addAngleRadians (Radians r1) r2 = Radians $ r1 + r2

-- | Add turns to angle
addAngleTurns :: (Floating r, Real t) => Angle r -> t -> Angle r
addAngleTurns ang turn = addAngle ang $ angleFromTurns turn

-- Trigonometric functions


-- | Sine of the angle
sinAngle :: (Floating a) => Angle a -> a
sinAngle = sin . angleValueRadians

-- | Cosine of the angle
cosAngle :: (Floating a) => Angle a -> a
cosAngle = cos . angleValueRadians

-- | Tangent of the angle
tanAngle :: (Floating a) => Angle a -> a
tanAngle = tan . angleValueRadians

-- | Cotangent of the angle
cotAngle :: (Floating a) => Angle a -> a
cotAngle = recip . tan . angleValueRadians


-- Inverse trigonometric functions


-- | Create angle from inverse sine
asinAngle :: (Floating a) => a -> Angle a
asinAngle = Radians . asin

-- | Create angle from inverse cosine
acosAngle :: (Floating a) => a -> Angle a
acosAngle = Radians . acos

-- | Create angle from inverse tangent
atanAngle :: (Floating a) => a -> Angle a
atanAngle = Radians . atan

-- | Create angle from inverse cotangent
acotAngle :: (Floating a) => a -> Angle a
acotAngle x = Radians $ (pi/2) - (atan x)


-- | Create angle from atan2
atan2Angle :: (Floating a, RealFloat a) => a -> a -> Angle a
atan2Angle y x = Radians $ atan2 y x

