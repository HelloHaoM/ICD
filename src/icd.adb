with ICD;
with HRM;
with ImpulseGenerator;
With Network;

package body ICD is 
   
   --The interval of tachycardia singal
   TaTickInterval : Integer;
   
   --The above of tachycardia rate
   TaRate : Measures.BPM;
   
   --The number of time for tachycardia have run
   TaTime : Integer := 0;
    
   procedure Init(Icd : out ICDType; Hrm : out HRMType;
                  Net : out Network; KnownPrincipals : access PrincipalArray,
                  Generator : out GeneratorType) is
   begin
      Icd.IsOn := False;
      HRM.Init(Hrm);
      Network.Init(Net,KnownPrincipals);
      ImpulseGenerator.Init(Generator);
   end Init;

   procedure On(Icd : out ICDType; Hrm : in HRM.HRMType) is
   begin
     --Get the initial reading form hrm
     Icd.IsOn := True;
     HRM.GetRate(Hrm, Icd.Rate);
   end On;

   procedure Off(Icd: in out ICDType) is
   begin
      Icd.IsOn := False;
   end Off;
   
   procedure SetTaUpperBound(Rate : out Measures.BPM) is
   begin
      if Icd.IsOn then
         ICD.UPPER_BOUND := Rate;
      end if;
   end SetTaUpperBound;
   
   procedure SetVentriceJoules(VJoulse : Measures.Joules) is
   begin
      if Icd.IsOn then
         ICD.VENTRICLE_JOULES := VJoules;
      end if;
   end SetVentriceJoules;
   
     

   function IsOn(Icd : in ICDType) return Boolean is
   begin
      return Icd.IsOn;
   end IsOn;
   
   function DetectVentricle(Icd : out ICDType; History : out Network.RateHistory) return Boolean is
      --The result of rate sum
      CalculateResult : Integer;
      HistoryPos : Integer := History'Last;
      CalculateResult := abs(Icd.Rate - History(HistoryPos).Rate);
      
      --Calculate t - (t-6) + 1 times
      fot I in Integer range reverse HistoryPos-6..HistoryPos loop
         Temp : Integer := History(I).Rate - History(I-1).Rate;
         CalculateResult := CalculateResult + Temp;
      end loop;
      
      --return the result
      if(CalculateResult / 6 >= 10) then
         return True;
      else
         retun False;
   end DetectVentricle;
   
   function GetImpulseType(Icd : in out ICDType; 
                           History : out Network.RateHistory) return ImpulseType is
      begin
         if(Icd.Rate > ICD.UPPER_BOUND) then
            TaRate := Icd.Rate + 15;
            TaTickInterval := 600 / taRate;
            return ImpulseType.normal;
         elsif(DetectVentricle(Icd, History)) then
            return ImpulseType.ventrice;
         else
            return ImpulseType.normal;
         end if;
      end GetImpulseType;
   
   procedure Tick(Icd : in out ICDType; Hrm : in out HRM.HRMType; History : out Network.RateHistory; Generator : in out ImpulseGenerator) is
      begin
         if Icd.IsOn then         
            --Get rate from hrm
            HRM.GetRate(Hrm, Icd.Rate);
            
            --Get the detected result
            if(TaTime = 0) then
             Result : ImpulseType := GetImpulseType(Icd, History);
            end if;
            
            if(Result = ImpulseType.tachycardia or TaTime < 10) then
               
               if(TaTime mod TaTickInterval = 0) then
                  ImpulseGenerator.SetImpulse(2);
                  TaTime := TaTime + 1;
                  if(TaTime = 10) then
                     TaTime := 0;
                  end if;
               end if;
               
            elsif(Result = ImpulseType.ventricle) then
               ImpulseGenerator.SetImpulse(ICD.VENTRICLE_JOULES);
               
            else
               ImpulseGenerator.SetImpulse(0);
            end if;
               
         else
            --If the monitor is not on, return 0 for both values
            Icd.Rate = Measures.BPM'First;
         end if;
         
      end Tick;
                                                 
            
end ICD;
