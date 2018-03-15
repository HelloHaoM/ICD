with Measures;
with HRM;
with Network;

-- This package is use to defined a icd model
-- The main purpose is to calculate the impulse to be delivered
-- There are three main type: tachycardia, ventricle and neither

package ICD is 
   
   --The upper bound of tachycardia
   UPPER_BOUND : Measures.BPM := 100;
   
   --The initial number of joules for ventricle
   VENTRICLE_JOULES : Measures.Joules := 30;
   
   --The type for three situtions
   type ImpulseType is (normal,
                        tachycardia,
                        ventricle);
   
   --The record type for a icd model
   type ICDType is
      record
         --The measure heart rate for the heart
         Rate : Measures.BPM;
         
         --The measure impulse joule
         Joules : Measures.Joules;
         
         --Indicates whether the icd model on or not
         IsOn : Boolean;
         
      end record;
   
   -- Create and initialise a icd
   procedure Init(Icd : out ICDType; Hrm : out HRMType;
                  Net : out Network; KnownPrincipals : access PrincipalArray,
                  Generator : out GeneratorType);
   
   -- Turn on the icd and get a first message from network
   procedure On(Icd : out ICDType; Hrm : in HRM.HRMType);
   
   --Turn off the icd
   procedure Off(Icd : in out ICDType);
   
   --Set tachycardia upper bound
   procedure SetTaUpperBound(Icd : in ICDType; Rate : out Measures.BPM);
   
   --Set joules of ventrice
   procedure SetVentriceJoules(VJoulse : Measures.Joules);

   -- Get the status of the icd (on/off)
   function IsOn(Icd : in ICDType) return Boolean;

   --Detect whether situation is ventricle fibrillation
   function DetectVentricle(Icd : out ICDType; History : out Network.RateHistory)
     return Boolean;                
   
   --Get the impulse type
   function GetImpulseType(Icd : in out ICDType; History : out Network.RateHistory) 
     return ImpulseType;                  

   --Tick the clock. reading the heart rate from hrm and message from network
   procedure Tick(Icd : in out ICDType; Hrm : in out HRM.HRMType; 
                  Generator : in out ImpulseGenerator);
         
end ICD;

