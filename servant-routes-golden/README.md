# servant-routes-golden

This package lets us define [Golden tests](https://ro-che.info/articles/2017-12-04-golden-tests) using the `HasRoutes` class from [servant-routes](https://github.com/fpringle/servant-routes/blob/main/servant-routes#readme) and the [hspec-golden](https://hackage.haskell.org/package/hspec-golden) library.

See `Servant.API.Routes.Golden` for reference documentation.

## Example

```haskell
-- file src/API.hs
module API where

import Servant.API

type UserAPI =
  "users"
    :> ( "list" :> Get '[JSON] [User]
          :<|> "create" :> ReqBody '[JSON] UserCreateData :> Post '[JSON] UserID
          :<|> "detail" :> QueryParam' '[Required] "id" UserID :> Get '[JSON] User
       )

-- file test/APISpec.hs

module APISpec where

import API
import Servant.API.Routes.Golden
import Hspec

spec :: Spec
spec =
  it "UserAPI" $ goldenRoutes @UserAPI (show ''UserAPI)
```

We can run `cabal test` to generate the starting "golden file":

```bash
$ cabal test
API
  UserAPI [✔]
    First time execution. Golden file created.
```

Of course, if we run the test again, it should pass:

```bash
$ cabal test
API
  UserAPI [✔]
    Golden and Actual output didn't change
```

But let's say we change the API definition slightly:

```diff
type UserAPI =
  "users"
    :> ( "list" :> Get '[JSON] [User]
-          :<|> "create" :> ReqBody '[JSON] UserCreateData :> Post '[JSON] UserID
+          :<|> "create-new" :> ReqBody '[JSON] UserCreateData :> Post '[JSON] UserID
          :<|> "detail" :> QueryParam' '[Required] "id" UserID :> Get '[JSON] User
       )
```

Then when we run the tests again:

```bash
$ cabal test
API
  UserAPI [✘]
    Files golden and actual not match

Failures:

  test/APISpec.hs:9:3: 
  1) Servant.API.Routes.Golden UserAPI
       expected: {
                     "/users/create": {
                         "POST": {
                             "auths": [],
                             "description": null,
                             "method": "POST",
                             "params": [],
                             "path": "/users/create",
                             "request_body": "UserCreateData",
                             "request_headers": [],
                             "response": {
                 @@ 45 lines omitted @@
                 
        but got: {
                     "/users/create-new": {
                         "POST": {
                             "auths": [],
                             "description": null,
                             "method": "POST",
                             "params": [],
                             "path": "/users/create-new",
                             "request_body": "UserCreateData",
                             "request_headers": [],
                             "response": {
                 @@ 45 lines omitted @@
```

This forces us to either:

- acknowledge that the golden files should be updated, and do so by running the [hgold CLI](https://github.com/stackbuilders/hspec-golden?tab=readme-ov-file#install-cli), or
- realise that our changes resulted in a change to the API which we didn't anticipate, so we have to fix them.
