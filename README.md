# servant-routes

-- intro description

## Motivation

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
        "response": "[User]",
        "response_headers": []
    },
    {
        "auths": [],
        "method": "POST",
        "params": [],
        "path": "/users/create",
        "request_body": "UserCreateData",
        "request_headers": [],
        "response": "UserID",
        "response_headers": []
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
        "response": "User",
        "response_headers": []
    },
    {
        "auths": [],
        "method": "GET",
        "params": [],
        "path": "/transactions/<TransactionID>",
        "request_body": null,
        "request_headers": [],
        "response": "Transaction",
        "response_headers": [
            {
                "name": "x-request-id",
                "type": "RequestID"
            }
        ]
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
        "response": "UserID",
        "response_headers": []
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
        "response": "[User]",
        "response_headers": []
    },
    {
        "auths": [],
        "method": "POST",
        "params": [],
        "path": "/users/create",
        "request_body": "UserCreateData",
        "request_headers": [],
        "response": "UserID",
        "response_headers": []
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
        "response": "User",
        "response_headers": []
    },
    {
        "auths": [],
        "method": "GET",
        "params": [],
        "path": "/transactions/<TransactionID>",
        "request_body": null,
        "request_headers": [],
        "response": "Transaction",
        "response_headers": [
            {
                "name": "x-request-id",
                "type": "RequestID"
            }
        ]
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
        "response": "UserID",
        "response_headers": []
    }
]
```

</details>

#### Writing `HasRoutes` instances for custom combinators

For the most part you'll be able to use `getRoutes` out of the box, without worrying about `HasRoutes` and other internals.
But sometimes you'll want to extend `servant` with your own custom combinators, writing `HasServer` or `HasClient` for them etc.
Then you'll also need to write a `HasRoutes` instance.

Let's write a `HasRoutes` instance for the `Replay` combinator in William Yao's
[Writing servant combinators for fun and profit](https://williamyaoh.com/posts/2023-02-28-writing-servant-combinators.html#:~:text=Example%3A%20Returning%20a%20header%20with%20a%20%22replay%22%20path).
The `HasServer` instance tells us that the effect of `Replay :> api` is to inherit the behaviour of `api`, but add an `X-Replay-Path` header of type `ByteString` to the response.

As far as our `HasRoutes` instance is concerned, this means that we need to:

1. Call `getRoutes` on `api` to get the list of un-modified routes
2. Add a `HeaderRep` to the `routeResponseHeaders` field of each route, with name `X-Replay-Path` and type-rep `ByteString`.

```haskell
data Replay

instance HasRoutes api => HasRoutes (Replay :> api) where
  getRoutes =
    let apiRoutes = getRoutes @api
        replayHeader = mkHeaderRep @"X-Replay-Path" @ByteString
        addHeader route = route & routeHeaderReps %~ (replayHeader :)
    in  addHeader <$> apiRoutes
```

We can test the implementation on `ServantAPI` from above:

```haskell
ghci> BL.putStrLn . encodePretty $ getRoutes @(Replay :> ServantAPIWithNamedRoutes)
```

<details>
<summary>Click to see JSON output</summary>

Note that each route is the same as above, but with an extra `response_header` `{"name": "X-Replay-Path", "type": "ByteString"}`:
  
```json
[
    {
        "auths": [],
        "method": "GET",
        "params": [],
        "path": "/users/list",
        "request_body": null,
        "request_headers": [],
        "response": "[User]",
        "response_headers": [
            {
                "name": "X-Replay-Path",
                "type": "ByteString"
            }
        ]
    },
    {
        "auths": [],
        "method": "POST",
        "params": [],
        "path": "/users/create",
        "request_body": "UserCreateData",
        "request_headers": [],
        "response": "UserID",
        "response_headers": [
            {
                "name": "X-Replay-Path",
                "type": "ByteString"
            }
        ]
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
        "response": "User",
        "response_headers": [
            {
                "name": "X-Replay-Path",
                "type": "ByteString"
            }
        ]
    },
    {
        "auths": [],
        "method": "GET",
        "params": [],
        "path": "/transactions/<TransactionID>",
        "request_body": null,
        "request_headers": [],
        "response": "Transaction",
        "response_headers": [
            {
                "name": "X-Replay-Path",
                "type": "ByteString"
            },
            {
                "name": "x-request-id",
                "type": "RequestID"
            }
        ]
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
        "response": "UserID",
        "response_headers": [
            {
                "name": "X-Replay-Path",
                "type": "ByteString"
            }
        ]
    }
]
```

</details>

---

Credits

CA shoutout
