with Measures;
with HRM;
with Network;
with Heart;
with ImpulseGenerator;
with Principal;

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
   
   --length of history record for heart rate
   HIS_LENGTH : constant Integer := 10;
   
   -- history record
   type His_RecordType is 
     array (Integer range 1..HIS_LENGTH) of Network.RateRecord;
   
   --The record type for a icd model
   type ICDType is
      record
       
         --The measure heart rate for the heart
         Rate : Measures.BPM;
         
         --The history heart record
         His_rec : His_RecordType;
         
         --Current history record length
         Current_index : Integer := 0;
         
         --The measure impulse joule
         Joules : Measures.Joules;
         
         --Indicates whether the icd model on or not
         IsOn : Boolean;
         
      end record;
   
   -- Create and initialise an icd
   procedure Init(Icd_tor : out ICDType;
                  Monitor : out HRM.HRMType;
                  Generator : out ImpulseGenerator.GeneratorType);
   
   -- Turn on the icd and get a first message from network
   procedure On(Icd_tor : out ICDType;
                Monitor : out HRM.HRMType;
                Generator : out ImpulseGenerator.GeneratorType;
                Hrt : in Heart.HeartType);
   
   --Turn off the icd
   procedure Off(Icd_tor : out ICDType;
                Monitor : out HRM.HRMType;
                Generator : out ImpulseGenerator.GeneratorType);
   
   --Set tachycardia upper bound
   procedure SetTaUpperBound(Icd_tor : in out ICDType; 
                             Rate : in Measures.BPM);
   
   --Set joules of ventrice
   procedure SetVentriceJoules(Icd_tor : in out ICDType; 
                               VJoulse : in Measures.Joules);

   -- Get the status of the icd (on/off)
   function IsOn(Icd_tor : in ICDType) return Boolean;

   --Detect whether situation is ventricle fibrillation
   function DetectVentricle(Icd_tor : in out ICDType)
                            return Boolean;                
   
   --Detect tachycardia eixsts or not
   function DetectTachycardia(Icd_tor : in ICDType)
                              return Boolean;
   
   --Get the impulse type
   function GetImpulseType(Icd_tor : in out ICDType) 
                           return ImpulseType;   
   
   --generate a rateHistory to send to net
   function GetRateHis_toSend(Icd_tor : in ICDType) 
                              return Network.RateHistory;
   
   --Add a new heart record
   procedure AddRecord(Icd_tor : in out ICDType; 
                       Current_time : in Measures.TickCount);
   
   --check and response net message
   procedure ResponseNetMsg(Icd_tor : in out ICDType;
                            Monitor : in out HRM.HRMType;
                            Generator : in out ImpulseGenerator.GeneratorType;
                            Hrt : in out Heart.HeartType;
                            MsgAvailable : out Boolean;
                            Net : in out Network.Network);

   --Tick the clock. reading the heart rate from hrm and message from network
   procedure Tick(Icd_tor : in out ICD.ICDType;
                  Monitor : in out HRM.HRMType;
                  Generator : in out ImpulseGenerator.GeneratorType;
                  Hrt : in out Heart.HeartType;
                  Current_time : in Measures.TickCount;
                  NetAvailable : in out Boolean;
                  Net : in out Network.Network);
         
end ICD;
