# Server store organisation
This an attempt at keeping a trace of how the store is used and were a piece of
data should be saved.

The ideal would be to keep it up to date as much as possible and use it to
restrain the data to go in all directions.

## Summary of the object tree

```
/
├── users
│   ├── user_1 [username; password; device_ids; ...]
│   ├── ...
│   └── user_x
├── devices
│   ├── device_1 [user_id; token_id; ...]
│   ├── ...
│   └── device_x
└── tokens
    ├── token_1 [issued_at; expires_at; device_id]
    ├── ...
    └── token_x
```

## Folders

### User
Contains the informations about a user.
It keeps the list of the devices associated with it.

### Devices
Contains the informations about a device.
It keeps the id of the token and the user associated with it.

### Tokens
Contains the informations about a token.
It keeps the id of the device associated with it.

#### Aliases
Contains the informations about room aliases

#### Rooms
Contains the informations about a room.

### About the relationship between users, devices and tokens
As of today, the project is using irmin as a storage tool and it should provide
some kind of assurance against potential broken links between the different kind
of data.

This cases could happend if the execution of a function was stopped halfway for
example, therefore not ensuring that all operations were succesfull. In the case
of token deletion for instance, we could imagine a case were it would lead to
ghost tokens which links to are all deleted but which are granting access to a
authentication request.

This kind of scenario is obviously to be avoided at all cost, and even if
irmin's commit mechanism should avoid those uncertain states, it would be better
to think beforehand how to hierarchise those 3 objects.

The main idea I'm going to stick too for now is to always consider the user
first as the real information, then, if it does not invalidate the device
information, the device additionnal informations are considered correct as well
and we repeat that for the token if it is not invalidated by the device. This
also implies that modifications should always be done on the user first, then
it's devices and then the tokens.

e.g.:
We have a user `user_1` who has a device `device_1` and which has the token
`token_1`.
```
user_1
^    |
|    v
device_1
^    |
|    v
token_1
```
Now let's consider that the token `token_1` is invalidated and replaced by the a
new token `token_2`, but the request is for any reason stopped halfway, after
the device has been updated, but before the `token_1` has been removed.
```
expected:         | result:
user_1            | user_1
^    |            | ^    |
|    v            | |    v
device_1          | device_1 --------
^    |            | ^          |    ^
|    v            | |          v    |
token_2           | token_1    token_2
```
If a user attempts to user the `token_1`, we should first retrieve it's associated
device (aka fetch `device_1`), then retrieve the user of the device (aka
`user_1`).
Now we go down the tree, first checking if the `device_1` is still a valid
device, which it is in our example. Once this check is done, we go further down
and try to check of the `token_1` is still associated to the `device_1`, which
it is not.

The authentication should then be refused and if possible the `token_1` should
be removed.

By behaving this, we are forced to always go all the way up to the user when we
use a token, but it should discard all the potential unkown states if a problem
occured after we started the modifications on the store.
