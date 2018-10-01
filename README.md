# scala-fsm-phantom
Finite State Machine Implemented in Scala using Phantom Types so there is no need for tests of basic transitions between states because compiler does not allow invalid transitions.

Instruction FSM is actually two FSMs that should be cooperating:
One for message state of the Instruction (SWIFT):
  <pre>
    New              =>       Failed
     ||
     \/  
  Published                      ||
     ||                          \/
     \/  
  Instructed 
     ||
     \/
  Cancel Submitted    =>     Not Instructed
     ||
     \/  
  Cancelled
  </pre>
  
  One for confirmation  state (it is not possible to cancle):
  <pre>
  Unconfirmed             
     ||
     \/  
  Confirmed <==
     ||       ||
     ==========
  </pre>
  
