Erlymob HTTP API
================

Mob Stuff
---------

*Path*: `/mob`

*Function*: returns the object representation of the specified mob, including
          the original tweet info, plus parsed location and time data and
          participants who have RSVP’d.

*Method*: GET

*Params*:
    - `token` (*optional*): the access token for the twitter API for the user
                          whose data we wish to retrieve. If there’s no token
                          given, we can return the mob, but the "going" flag
                          will be missing.
    - `id`: the ID of the mob we want to retrieve.

*Returns*:
    ```javascript
    {
        "id": 12345,
        "tweet": "@erlymob Meet at Joe's in half an hour for free hot dogs!",
        "user": "tomheinan",
        "where": {
            "latitude": 45.091778,
            "longitude": -64.361923,
        },
        "when": 1339115400,
        "rsvps": 3,
        "going": false
    }
    ```


*Path*: `/mobs`

*Function*: returns an array of all the upcoming mobs for which a user is eligible.

*Method* GET

*Params*: as above, but sans `id`, and the token is not optional.
    - `token`: the access token for the twitter API for the user whose data we
               wish to retrieve.

*Returns*: as above, but as an array.


*Path*: `/rsvp`

*Function*: toggles the `going` tag on a particular mob, which signifies if I’m
          attending this mob or not.

*Method* POST

*Params*:
    - `token`: the access token for the twitter API for the user whose data we
               wish to retrieve.
    - `id`: the ID of the mob we want to retrieve.

*Returns*:
    ```javascript
    {
        "going": true
    }
    ```

User Stuff
----------

*Path*: `/set_loc`

*Function*: sets the currently logged in user’s location.

*Method* POST

*Params*:
    - `token`: the access token for the twitter API for the user whose data we
               wish to retrieve.
    - `latitude`: latitude where the mob will take place.
    - `longitude`: longitude where the mob will take place.


*Path*: `/get_loc`

*Function*: gets the currently logged in user’s most recent location.

*Method* GET

*Params*:
    - `token`: the access token for the twitter API for the user whose data we
               wish to retrieve.

*Returns*:
    ```javascript
    {
        "latitude": 33.786647,
        "longitude": -84.37853
    }
    ```