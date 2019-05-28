# Change Log Details 0.24.1

## Make `Server` instances composable

Currently when a `Server` instance wants to delegate the `update`
to another `Server` instance, the **reply** type will not change from
e.g. `Reply inner reply` to `Reply (Outer inner) outerReply`
 
This is solved by introducing a third parameter to `sendReply`,
a `Serializer x` that is isomorphic to
 
    x -> StrictDynamic    ::    Serializer x
    
By choosing `Reply inner reply` for `x` in `sendReply` we get:   
 
    sendReply :: Serializer (Reply inner reply) -> RequestOrigin inner reply -> reply -> Eff e ()

This is type safe for nested `Pdu`s. Assume 

    data instance Pdu Outer r where
     ToInner :: Pdu Inner ('Synchronous Float) -> Pdu Outer ('Synchronous Float)
    
    data instance Pdu Inner r where
     FooHoo :: Pdu Inner ('Synchronous Float)
     
Remeber that *wrong* replies will not be received by the calling process,
waiting for a `Reply Outer ('Synchronous Float)` message.

Now imagine if we implemented a `Server` instance for `Outer`, the compiler
can assure, that `sendReply` is always called with the correct `Reply`. 

Currently there is no `Serializer` so we have:

    onEvent (OuterInit innerInit) (OnCall origin (ToInner innerPdu)) = 
      onEvent innerInit (OnCall (coerce origin) innerPdu)

Note, we have to coerce the `RequestOrigin Outer Float` to 
`RequestOrigin Inner Float`.

If we would let the composed/nested/inner `Server` instances' `onEvent` method
simply call `sendReply`, it won't call it with the expected `Reply` type,
because from the `Inner` instance we have:
     
    onEvent :: Init Inner e -> Event Inner -> Eff e ()
   
And for `Outer` we have:   

    onEvent :: Init Outer e -> Event Outer -> Eff e ()
    
In `Event` the `OnCall` clause has this type:

    data Event a where 
      OnCall 
        :: RequestOrigin a r 
        -> Pdu a ('Synchronous r) 
        -> Event a     

... So for the `OnCall` passed to `Inner`s `onEvent` we get:

    onEvent _ 
     (OnCall  
       (o :: RequestOrigin Inner Float) 
       (FooHoo :: Pdu Inner ('Synchronous Float))) = sendEvent s o 3.142 
       
... and the message received by the client is `Reply Inner Float`,
but the client is only selecting for `Reply` **`Outer`** `Float`!
        
The client dies with a timeout.   

But thanks to the `Serializer` parameter to `sendReply` and `OnCall` 
we get type safety: 

    onEvent (OuterInit innerInit) (OnCall serializer origin (ToInner innerPdu)) = 
      onEvent innerInit (OnCall serializer (coerce origin) innerPdu) 
                                ^^^^^^^^^^^^^^^^^^^^^^^^^^^
                         Error: Couldn't match type `Outer` with `Inner`
                                Expected type: Serializer (Reply Inner r)
                                  Actual type: Serializer (Reply Outer r)        

Because in `onEvent` the `OnCall` clause gets this type at the `Outer` instance:

      OnCall 
        :: Serializer (Reply Outer Float) 
        -> RequestOrigin Outer Float 
        -> Pdu Outer ('Synchronous Float) 
        -> Event Outer   

... and at the `Inner` instance:

      OnCall 
        :: Serializer (Reply Inner Float) 
        -> RequestOrigin Inner Float 
        -> Pdu Inner ('Synchronous Float) 
        -> Event Inner   


 So for the `OnCall` passed to `Inner`s `onEvent` we get:

    onEvent _ 
     (OnCall 
      (s :: Serializer (Reply Inner Float))) 
      (o :: RequestOrigin Inner Float) 
      (r :: Pdu Inner ('Synchronous Float)) = sendEvent s o 3.142
      
And to solve the type error we must apply the appropriate conversion
to the `Serializer`, in this case `coerce` is enough, since the 
`protocol` parameter is only a phantom type parameter:

    onEvent (OuterInit innerInit) (OnCall serializer origin (ToInner innerPdu)) = 
      onEvent innerInit (OnCall (contramap coerce serializer) (coerce origin) innerPdu) 
                                ^^^^^^^^^^^^^^^^^^^^^^^^^^^
