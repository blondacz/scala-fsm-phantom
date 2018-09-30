# scala-fsm-phantom
Finite State Machine Implemented in Scala using Phantom Types so there is no need for tests of basic transitions between states because compiler does not allow invalid transitions.

Basic states for Instrurtion (SWIFT) are:
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
  
