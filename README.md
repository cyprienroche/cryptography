# Project Title

A Haskell implementation of a widely-used public key encryption algorithm and a symmetric key algorithm.

## Getting Started

### Prerequisites

To install before running:

```
GHC compiler for Haskell programs
```

### Installing

Run the following command once in the project directory

```
ghci Crypto.hs
```

rsaEncrypt takes two parameters; a *Int* to be encrypted and a tuple which is the *key*. rsaDecrypt is the same and decrypts the encrypted *Int*. To generate a pair of keys, use the genKeys function which given two distinct prime numbers runs the RSA key generation algorithm and returns a key pair.

```
*Crypto> genKeys 7 13
((5,91),(29,91))

*Crypto> rsaEncrypt 10 (5, 91)
82

*Crypto> rsaDecrypt 82 (29, 91)
10
```
ecbEncrypt takes two parameters; a character which is the *key* and a *message* as a String. It moves every letter down the alphabet by a certain amount determined by the *key*. ecbDecrypt is the same and decrypts the encrypted *message*.

```
*Crypto> ecbEncrypt 'r' "cryptography"
"tipgkfxirgyp"

*Crypto> ecbDecrypt 'r' "tipgkfxirgyp"
"cryptography"
```

cbcEncrypt takes three parameters: a *key*, an initial *offset* and a *message*. Each character is encrypted depending on the previous characters's ecnryption. Therefore it is harder to find patterns and decrypt the message by looking at common characters (unlike ecbEncrypt). cbcDecrypt is the same and decryptis the encrypted *message*.

```
*Crypto> cbcEncrypt 'r' 'm' "cryptography"
"fncisxuctzxm"

*Crypto> cbcDecrypt 'r' 'm' "fncisxuctzxm"
"cryptography"
```
## Running the tests

Running the tests:

```
runghc Tests.hs
```

## Acknowledgments

* **Imperial College** London
