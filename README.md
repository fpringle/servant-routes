# servant-routes

This package alllows us to automatically convert type-level Servant representations of APIs to concrete term-level representations.

See `Servant.API.Routes` for in-depth documentation.

## Contents

* [Motivation](#motivation)
* [Examples](#examples)
    * [Basic usage with servant combinators](#basic-usage-with-servant-combinators)
    * [Same example using NamedRoutes](#same-example-using-namedroutes)
    * [Writing HasRoutes instances for custom combinators](#writing-hasroutes-instances-for-custom-combinators)
* [Back story](#back-story)


## Motivation

Refactoring Servant API types is quite error-prone, especially when you have to move
around lots of `:<|>`s and `:>`s.  So it's very possible that the route structure could
change in that refactoring, _without being caught by the type-checker_.

The `HasRoutes` class could help as a "golden test" - run `getRoutes` before and after the refactor, and if they give the same
result you can be much more confident that the refactor didn't introduce difficult bugs.

Another use-case is in testing: some Haskellers use type families to modify Servant APIs, for example
to add endpoints or authorisation headers. Types are hard to test, but terms are easy. Use `HasRoutes`
and run your tests on `Routes`.


## Examples

#### Basic usage with `servant` combinators:

```haskell
type ServantAPI =
  "users"
    :> ( "list" :> Get '[JSON] [User]
          :<|> "create" :> ReqBody '[JSON] UserCreateData :> Post '[JSON] UserID
          :<|> "detail"
            :> QueryParam' '[Required] "id" UserID
            :> Header' '[Required] "x-api-key" ApiKey
            :> Get '[JSON] User
       )
    :<|> "transactions"
      :> ( Capture "id" TransactionID
            :> Get '[JSON] (Headers '[Header "x-request-id" RequestID] Transaction)
         )
    :<|> "admin"
      :> BasicAuth "admin" User
      :> ( "users" :> "delete" :> CaptureAll "ids" UserID :> Delete '[JSON] UserID
         )
```

```haskell
ghci> printRoutes @ServantAPI
GET /users/list
POST /users/create
GET /users/detail?id=<UserID>
GET /transactions/<TransactionID>
DELETE /admin/users/delete/<[UserID]>

ghci> BL.putStrLn . encodePretty $ getRoutes @ServantAPI -- using aeson-pretty
```

<details>
<summary>Click to see JSON output</summary>
  
```json
[
    {
        "auths": [],
        "method": "GET",
        "params": [],
        "path": "/users/list",
        "request_body": null,
        "request_headers": [],
        "response": {
            "headers": [],
            "type": "[User]"
        }
    },
    {
        "auths": [],
        "method": "POST",
        "params": [],
        "path": "/users/create",
        "request_body": "UserCreateData",
        "request_headers": [],
        "response": {
            "headers": [],
            "type": "UserID"
        }
    },
    {
        "auths": [],
        "method": "GET",
        "params": [
            {
                "name": "id",
                "param_type": "UserID",
                "type": "SingleParam"
            }
        ],
        "path": "/users/detail",
        "request_body": null,
        "request_headers": [
            {
                "name": "x-api-key",
                "type": "ApiKey"
            }
        ],
        "response": {
            "headers": [],
            "type": "User"
        }
    },
    {
        "auths": [],
        "method": "GET",
        "params": [],
        "path": "/transactions/<TransactionID>",
        "request_body": null,
        "request_headers": [],
        "response": {
            "headers": [
                {
                    "name": "x-request-id",
                    "type": "RequestID"
                }
            ],
            "type": "Transaction"
        }
    },
    {
        "auths": [
            "Basic admin"
        ],
        "method": "DELETE",
        "params": [],
        "path": "/admin/users/delete/<[UserID]>",
        "request_body": null,
        "request_headers": [],
        "response": {
            "headers": [],
            "type": "UserID"
        }
    }
]
```

</details>

#### Same example using `NamedRoutes`:

```haskell
data UserAPI mode = UserAPI
  { list :: mode :- "list" :> Get '[JSON] [User]
  , create :: mode :- "create" :> ReqBody '[JSON] UserCreateData :> Post '[JSON] UserID
  , detail ::
      mode
        :- "detail"
          :> QueryParam' '[Required] "id" UserID
          :> Header' '[Required] "x-api-key" ApiKey
          :> Get '[JSON] User
  }
  deriving (Generic)

newtype TransactionAPI mode = TransactionAPI
  { get ::
      Capture "id" TransactionID
        :> Get '[JSON] (Headers '[Header "x-request-id" RequestID] Transaction)
  }
  deriving (Generic)

newtype AdminAPI mode = AdminAPI
  { deleteUsers ::
      BasicAuth "admin" User
        :> "users"
        :> "delete"
        :> CaptureAll "ids" UserID
        :> Delete '[JSON] UserID
  }
  deriving (Generic)

data ServantAPIWithNamedRoutes mode = ServantAPIWithNamedRoutes
  { users :: mode :- "users" :> NamedRoutes UserAPI
  , transactions :: mode :- "transactions" :> NamedRoutes TransactionAPI
  , admin :: mode :- "admin" :> NamedRoutes AdminAPI
  }
  deriving (Generic)
```

```haskell
ghci> printRoutes @(NamedRoutes ServantAPIWithNamedRoutes)
GET /users/list
POST /users/create
GET /users/detail?id=<UserID>
GET /transactions/<TransactionID>
DELETE /admin/users/delete/<[UserID]>

ghci> BL.putStrLn . encodePretty $ getRoutes @(NamedRoutes ServantAPIWithNamedRoutes)
```

<details>
<summary>Click to see JSON output</summary>

Note this is the same as above, so we know we refactored `ServantAPI` to `ServantAPIWithNamedRoutes` correctly!
  
```json
[
    {
        "auths": [],
        "method": "GET",
        "params": [],
        "path": "/users/list",
        "request_body": null,
        "request_headers": [],
        "response": {
            "headers": [],
            "type": "[User]"
        }
    },
    {
        "auths": [],
        "method": "POST",
        "params": [],
        "path": "/users/create",
        "request_body": "UserCreateData",
        "request_headers": [],
        "response": {
            "headers": [],
            "type": "UserID"
        }
    },
    {
        "auths": [],
        "method": "GET",
        "params": [
            {
                "name": "id",
                "param_type": "UserID",
                "type": "SingleParam"
            }
        ],
        "path": "/users/detail",
        "request_body": null,
        "request_headers": [
            {
                "name": "x-api-key",
                "type": "ApiKey"
            }
        ],
        "response": {
            "headers": [],
            "type": "User"
        }
    },
    {
        "auths": [],
        "method": "GET",
        "params": [],
        "path": "/transactions/<TransactionID>",
        "request_body": null,
        "request_headers": [],
        "response": {
            "headers": [
                {
                    "name": "x-request-id",
                    "type": "RequestID"
                }
            ],
            "type": "Transaction"
        }
    },
    {
        "auths": [
            "Basic admin"
        ],
        "method": "DELETE",
        "params": [],
        "path": "/admin/users/delete/<[UserID]>",
        "request_body": null,
        "request_headers": [],
        "response": {
            "headers": [],
            "type": "UserID"
        }
    }
]

```

</details>

#### Writing `HasRoutes` instances for custom combinators:

For the most part you'll be able to use `getRoutes` out of the box, without worrying about `HasRoutes` and other internals.
But sometimes you'll want to extend `servant` with your own custom combinators, writing `HasServer` or `HasClient` for them etc.
Then you'll also need to write a `HasRoutes` instance.

Let's write a `HasRoutes` instance for the `Replay` combinator in William Yao's
[Writing servant combinators for fun and profit](https://williamyaoh.com/posts/2023-02-28-writing-servant-combinators.html#:~:text=Example%3A%20Returning%20a%20header%20with%20a%20%22replay%22%20path).
The `HasServer` instance tells us that the effect of `Replay :> api` is to inherit the behaviour of `api`, but add an `X-Replay-Path` header of type `ByteString` to the response.

As far as our `HasRoutes` instance is concerned, this means that we need to:

1. Call `getRoutes` on `api` to get the list of un-modified routes
2. Add a `HeaderRep` to the `responseHeaders` field of all the possible responses of each route, with name `X-Replay-Path` and type-rep `ByteString`.

```haskell
data Replay

instance HasRoutes api => HasRoutes (Replay :> api) where
  getRoutes =
    let apiRoutes = getRoutes @api
        replayHeader = mkHeaderRep @"X-Replay-Path" @ByteString
        addHeader route = route & routeResponse . unResponses . traversed . responseHeaders %~ Set.insert replayHeader
    in  addHeader <$> apiRoutes
```

We can test the implementation on `ServantAPI` from above:

```haskell
ghci> BL.putStrLn . encodePretty $ getRoutes @(Replay :> ServantAPI)
```

<details>
<summary>Click to see JSON output</summary>

Note that each route is the same as above, but with an extra `response.header` `{"name": "X-Replay-Path", "type": "ByteString"}`:
  
```json
[
    {
        "auths": [],
        "method": "GET",
        "params": [],
        "path": "/users/list",
        "request_body": null,
        "request_headers": [],
        "response": {
            "headers": [
                {
                    "name": "X-Replay-Path",
                    "type": "ByteString"
                }
            ],
            "type": "[User]"
        }
    },
    {
        "auths": [],
        "method": "POST",
        "params": [],
        "path": "/users/create",
        "request_body": "UserCreateData",
        "request_headers": [],
        "response": {
            "headers": [
                {
                    "name": "X-Replay-Path",
                    "type": "ByteString"
                }
            ],
            "type": "UserID"
        }
    },
    {
        "auths": [],
        "method": "GET",
        "params": [
            {
                "name": "id",
                "param_type": "UserID",
                "type": "SingleParam"
            }
        ],
        "path": "/users/detail",
        "request_body": null,
        "request_headers": [
            {
                "name": "x-api-key",
                "type": "ApiKey"
            }
        ],
        "response": {
            "headers": [
                {
                    "name": "X-Replay-Path",
                    "type": "ByteString"
                }
            ],
            "type": "User"
        }
    },
    {
        "auths": [],
        "method": "GET",
        "params": [],
        "path": "/transactions/<TransactionID>",
        "request_body": null,
        "request_headers": [],
        "response": {
            "headers": [
                {
                    "name": "X-Replay-Path",
                    "type": "ByteString"
                },
                {
                    "name": "x-request-id",
                    "type": "RequestID"
                }
            ],
            "type": "Transaction"
        }
    },
    {
        "auths": [
            "Basic admin"
        ],
        "method": "DELETE",
        "params": [],
        "path": "/admin/users/delete/<[UserID]>",
        "request_body": null,
        "request_headers": [],
        "response": {
            "headers": [
                {
                    "name": "X-Replay-Path",
                    "type": "ByteString"
                }
            ],
            "type": "UserID"
        }
    }
]
```

</details>


## Back story

The scenario I described in [Motivation](#motivation) arose while I was working with @asheshambasta at [CentralApp](https://www.centralapp.com/),
one of my freelancing clients. The CentralApp backend is a distributed system comprising of 565 Servant endpoints across 7 different services
(plus several more services that aren't using Servant). Many of these endpoints are deeply nested, and as anyone familiar with Servant knows, debugging
error messages can be very frustrating. The best solution to this problem is to use [NamedRoutes](https://hackage.haskell.org/package/servant/docs/Servant-API.html#t:NamedRoutes)
and derive `HasServer` instances using `Generic`. I took on the task of refactoring _every one_ of those 565 endpoints to use `NamedRoutes` instead of
`:<|>` and `:>`.

Not only was this process fiddly and tedious, it was also potentially error-prone. The type-checker helps, of course, and will let you know if you accidentally
swapped 2 handler functions. However, what if you miss out or misspell a path part (e.g. `"api" :> ...`)? These have no effect on the `ServerT` instances, and thus
can't be caught by the type-checker. We wouldn't know if we made this kind of error until after deployment, when an endpoint would suddenly be at completely the wrong location,
or expecting `QueryParam`s with the names switched round, or many other bugs caused by human error.

Having worked with [servant-openapi3](https://hackage.haskell.org/package/servant-openapi3), I got the idea for a much simpler version in order to solve the above problem.
If I could use a similar mechanism to convert API types to term-level values describing the shape and semantics of all the routes of the API, I could compare the
representation _before_ and _after_ the refactoring. If the lists of routes were identical, and assuming my representation of the routes was accurate and expressive enough,
I could be confident that I wasn't introducing any subtle bugs.

Fortunately, this approach worked! After refactoring those 565 endpoints to use `NamedRoutes`, I ran `getRoutes` and compared the output to the output from the commit _before_
the refactor. I used [jdiff](https://github.com/networktocode/jdiff) to compare the outputs in JSON form. The comparison revealed that _one_ endpoint had a subtle bug:
it was missing a path part, as described above. And the type-checker didn't catch the mistake, which confirmed the need for this package!
Having fixed the mistake, I deployed the refactoring without a single issue.
