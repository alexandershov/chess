### 2020-04-03

**How to parse UCI commands with arguments?**
* ❌Data.List.isPrefixOf with guards on the whole string
    * 😡Harder to parse commands with optional arguments
* ❌Data.List.isPrefixOf with guards on words of the string
    * 😡Looks less elegant than pattern match
* ✅Pattern match on words
    * 🙂Looks clean
* ❌'p':'r':'e':'f':'i':'x'
    * 😡Come on! You're better than this!

**How to represent UCI commands with arguments?**

* ❌Position [String]
    * 😡Right now you don't need arguments
* ❌Position String
    * 😡Right now you don't need arguments
* ✅Position
    * 🙂Right now you don't need arguments

**Uci.parse or Uci.parseCommand?**

* ✅Uci.parse
    * 🙂Looks nice
    * 😡Parse what exactly?
* ❌Uci.parseCommand
    * 🙂Specific
    * 😡Type in name

**What are UCI types?**

* ❌Uci.Input & Uci.Output
    * 😡Uci.Command & Uci.Response are more specific
* ✅Uci.Command & Uci.Response
    * 🙂Clear names
    * 🙂Extra type checking: we can't return Uci.Command in response
* ❌Uci.Command for everything
    * 🙂Simple
    * 😡No type checking, we can return illegal Uci.Command (like go) in response
* ❌Uci.Line for everything
    * 😡Uci.Command is more specific name

### 2020-04-02


**What to use for tests?**

* ❌assert
    * 🙂Simple, nothing to install
    * 😡Bad error messages
* ✅Third-party test framework
    * 🙂Good error messages
    * 😡Adding some complexity

**Which framework to use?**

* ❌[tasty](https://github.com/feuerbach/tasty)
    * 🙂Active & popular
    * 😡Tests look ... not quite readable
* ❌[HUnit](https://github.com/hspec/HUnit)
    * 🙂Familiar interface
    * 😡77 stars on github
    * 😡Last commit 3 years ago
* ✅[hspec](https://github.com/hspec/hspec)
    * 🙂Active & popular
    * 🙂Familiar interface
    * 🙂Tests look nice and readable
    * 🙂Documentation is *really* user-friendly
* ❌[quickcheck](https://github.com/nick8325/quickcheck)
    * 🙂Cool idea
    * 😡Looks like overkill

**Should I use Haskell Stack?**

* ✅Yes
    * 🙂Using the wheel 
    * 😡Adding some complexity
* ❌No
    * 😡Reinventing the wheel