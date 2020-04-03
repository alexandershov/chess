### 2020-04-03

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