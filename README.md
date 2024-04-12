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

---

Credits
CA shoutout
