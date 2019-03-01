with ICD;
with HRM;
with ImpulseGenerator;
With Network;
with Principal;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body ICD is 
   
   --The interval of tachycardia singal
   TaTickInterval : Integer := 999;
   
   --The above of tachycardia rate
   TaRate : Measures.BPM;
   
   --The number of time for tachycardia have run
   TaTime : Integer := 0;
    
   procedure Init(Icd_tor : out ICDType;
                  Monitor : out HRM.HRMType;
                  Generator : out ImpulseGenerator.GeneratorType) is
   begin
      --when icd is init, hrm and impulsegenerator should be init too
      Icd_tor.IsOn := False;
      HRM.Init(Monitor);
      ImpulseGenerator.Init(Generator);
   end Init;

   -- procedure to set icd on
   procedure On(Icd_tor : out ICDType;
                Monitor : out HRM.HRMType;
                Generator : out ImpulseGenerator.GeneratorType;
                Hrt : in Heart.HeartType) is
   begin
   --when icd is on, monitor and generator all should be set on
     --Turn on icd and get the initial reading form hrm
      Icd_tor.IsOn := True;
      Put_Line("ICD is on");
      HRM.On(Monitor, Hrt);
      HRM.GetRate(Monitor, Icd_tor.Rate);
      ImpulseGenerator.On(Generator);
   end On;

   --procedure to set icd off
   procedure Off(Icd_tor : out ICDType;
                Monitor : out HRM.HRMType;
                Generator : out ImpulseGenerator.GeneratorType) is
   begin
      --Turn off icd, and monitor and generator all should be off
      Icd_tor.IsOn := False;
      Put_Line("ICD is off");
      HRM.Off(Monitor);
      ImpulseGenerator.Off(Generator);
   end Off;
   
   --procedure to set tachycardia upperbound
   procedure SetTaUpperBound(Icd_tor : in out ICDType; 
                             Rate : in Measures.BPM) is
   begin
   --only can change upperbound when icd is off
      if Icd_tor.IsOn = false then
         ICD.UPPER_BOUND := Rate;
         Put("set TaUpperBound new value: ");
         Put(Item => Rate);
         New_Line;
      end if;
   end SetTaUpperBound;
   
   procedure SetVentriceJoules(Icd_tor : in out ICDType; 
                               VJoulse : in Measures.Joules) is
   begin
   --only can change ventricle joules when icd is off
      if Icd_tor.IsOn = false then
         ICD.VENTRICLE_JOULES := VJoulse;
         Put("Set VentriceJoules new value: ");
         Put(Item => VJoulse);
         New_Line;
      end if;
   end SetVentriceJoules;
   
     
	--return icd's current state
   function IsOn(Icd_tor : in ICDType) return Boolean is
   begin
      return Icd_tor.IsOn;
   end IsOn;
   
   --this function is to detect ventricle according to the standart in specification
   function DetectVentricle(Icd_tor : in out ICDType) return Boolean is
      --The result of rate sum
      CalculateResult : Integer := 0;
	  --temporily store minus result
      Temp : Integer;
      Position : Integer := Icd_tor.Current_index;
      
   begin
      --There are no enough history data to caluate
      if(Position < 7) then
         return false;
      end if;
        
      --Calculate t - (t-5) + 1 times
      for I in (Position-5)..Position loop
         Temp := abs(Icd_tor.His_rec(I).Rate - Icd_tor.His_rec(I-1).Rate);
         CalculateResult := CalculateResult + Temp;
      end loop;
      
      --return the result, if the average > 10, it is true
      if(CalculateResult / 5 >= 10) then
         Put_Line("Ventricle detected!");
         return True;
      else
         return False;
      end if;
        
   end DetectVentricle;
   
   --this function is to detect tachycardia
   function DetectTachycardia(Icd_tor : in ICDType) return Boolean is 
   begin
   --it is true when current rate > upperbound
      if(Icd_tor.Rate > ICD.UPPER_BOUND) then
         Put_Line("Tachycardia detected!");
         return true; 
      else  
         return false;
      end if;
   end DetectTachycardia;
     
   -- this function return the type of current heart state
   function GetImpulseType(Icd_tor : in out ICDType) return ImpulseType is
      begin
         if(DetectTachycardia(Icd_tor)) then
            --Calculate the rate of impulse   
            TaRate := Icd_tor.Rate + 15;
			-- set the interval to ipulse joules
            TaTickInterval := (600 / TaRate) - 1;
            return ICD.tachycardia;
         elsif(DetectVentricle(Icd_tor)) then
            return ICD.ventricle;
         else
            return ICD.normal;
         end if;
      end GetImpulseType;
   
   --procedure to add a heart rate record when read a new one
   procedure AddRecord(Icd_tor : in out ICDType;
                       Current_time : in Measures.TickCount) is
      --used to temply store record
      tmp_rec : ICD.His_RecordType;
      tmp_hrtRec : Network.RateRecord;
      
   begin
      --create a tmp new heart record
      tmp_hrtRec.Rate := Icd_tor.Rate;
      tmp_hrtRec.Time := Current_time;
      
      if(Icd_tor.Current_index < ICD.HIS_LENGTH) then
         --Add a record to the history
         Icd_tor.Current_index := Icd_tor.Current_index + 1;
         Icd_tor.His_rec(Icd_tor.Current_index) := tmp_hrtRec;
      else
         --Delete the oldest one and update all the history
         tmp_rec(1..9) := Icd_tor.His_rec(2..10);
         tmp_rec(10) := tmp_hrtRec;
         Icd_tor.His_rec := tmp_rec;
      end if;
          
   end AddRecord;
   
   
   --generate a ratehistory type to send to net, length 5;
   --generate from the history record in icd ,which is 10 long
   function GetRateHis_toSend(Icd_tor : in ICDType) 
                              return Network.RateHistory is
      RateHis_toSend : Network.RateHistory;
   begin
   --need current record of icd has mroe than 4 records
      if(Icd_tor.Current_index >4) then
      for I in 1..5 loop
         --Send the history data   
         RateHis_toSend(6-I) := Icd_tor.His_rec(Icd_tor.Current_index + 1 - I);
      end loop;
      end if; 
      
      return RateHis_toSend;
      
   end GetRateHis_toSend;
  
      --check and response net message, before response , will check source is eligible or not
   procedure ResponseNetMsg(Icd_tor : in out ICDType;
                            Monitor : in out HRM.HRMType;
                            Generator : in out ImpulseGenerator.GeneratorType;
                            Hrt : in out Heart.HeartType;
                            MsgAvailable : out Boolean;
                            Net : in out Network.Network) is
      --temp message to store msg from net
      Tmp_message : Network.NetworkMessage;
      
      
   begin 
      Network.GetNewMessage(Net, MsgAvailable, Tmp_message);
   
      
      if(MsgAvailable) then
         Put("ICD received message.");
         New_Line;
         
         --If message is eligible from doctor or nurse
         -- response
         
         Network.DebugPrintMessage(Tmp_message);
         
         --Response depended on which message
         case Tmp_message.MessageType is
            --when is a ModeOn request, just set ICD on , both doctor and assistant can 
            when Network.ModeOn =>        
               if(Principal.HasRole(Tmp_message.MOnSource.all, Principal.Cardiologist) or
                    Principal.HasRole(Tmp_message.MOnSource.all, Principal.ClinicalAssistant)) then
                  Put("received ModeOn");New_Line;
                  ICD.On(Icd_tor,Monitor, Generator, Hrt);
               end if;   
                  --when is a ModeOn request, just set ICD off, both doctor and assistant can           
            when Network.ModeOff =>
               if(Principal.HasRole(Tmp_message.MOffSource.all, Principal.Cardiologist) or
                    Principal.HasRole(Tmp_message.MOffSource.all, Principal.ClinicalAssistant)) then
                  Put("received ModeOff");New_Line;
                  ICD.Off(Icd_tor, Monitor, Generator);
               end if;
               
               --when is a readHistoryRequest, send a response to net, both doctor and assiatant can
            when Network.ReadRateHistoryRequest =>
               if(Principal.HasRole(Tmp_message.HSource.all, Principal.Cardiologist) or
                    Principal.HasRole(Tmp_message.HSource.all, Principal.ClinicalAssistant)) then
                   Put("received readRateHistoryRequest.");New_Line;
                   Network.SendMessage(Net,
                                   (MessageType => Network.ReadRateHistoryResponse,
                                    HDestination => Tmp_message.HSource,
                                    History => ICD.GetRateHis_toSend(Icd_tor))
                                      );
               end if;
            -- when it's a changesetting request, change setting and send response to net, only doctor can
            when Network.ChangeSettingsRequest =>
               if(Principal.HasRole(Tmp_message.CSource.all, Principal.Cardiologist)) then
                  Put("received changesettingrequest.");New_Line;
                  ICD.SetTaUpperBound(Icd_tor, Tmp_message.CTachyBound);
                  ICD.SetVentriceJoules(Icd_tor, Tmp_message.CJoulesToDeliver);
               
                  Network.SendMessage(Net,
                                     (MessageType => Network.ChangeSettingsResponse,
                                      CDestination => Tmp_message.CSource)
                                     );
               end if;
               
             --when it's a readsettig request, send response to net, both doctor and assistant can  
            when Network.ReadSettingsRequest =>
               if(Principal.HasRole(Tmp_message.RSource.all, Principal.Cardiologist) or
                    Principal.HasRole(Tmp_message.RSource.all, Principal.ClinicalAssistant)) then
                  Put("received readsettingrequest.");New_Line;
                  Network.SendMessage(Net,
                                     (MessageType => Network.ReadSettingsResponse,
                                      RDestination => Tmp_message.RSource,
                                      RTachyBound => ICD.UPPER_BOUND,
                                      RJoulesToDeliver => ICD.VENTRICLE_JOULES)
                                     );
                end if;
               
            when others =>
               Put("Unknow message received.");
         end case;
         
      end if;    
      
   end ResponseNetMsg;
     
   --in a tick, monitor and generator will tick once
   --ICD will monitor net message, and response
   procedure Tick(Icd_tor : in out ICDType;
                  Monitor : in out HRM.HRMType;
                  Generator : in out ImpulseGenerator.GeneratorType;
                  Hrt : in out Heart.HeartType;
                  Current_time : in Measures.TickCount;
                  NetAvailable : in out Boolean;
                  Net : in out Network.Network) is
      
      Result : ImpulseType;
      Msg : Network.NetworkMessage;
      
   begin
      -- get message from net and response
      ICD.ResponseNetMsg(Icd_tor, Monitor, Generator, Hrt, NetAvailable, Net);
      
      if Icd_tor.IsOn then         
         --Get rate from hrm
         HRM.Tick(Monitor, Hrt);
         HRM.GetRate(Monitor, Icd_tor.Rate);
         ICD.AddRecord(Icd_tor, Current_time);

         --Get the detected result, when tatime is o, means one impulseGenerator treat is done
         --or nothing has done
         if(TaTime = 0) then
            Result := GetImpulseType(Icd_tor);
         end if;
         --when tachycardia is detected, imu;segenerator needs to impulse joules 10 times with 
         -- tainterval, which has been calculated when detecting tachycardia
         --so the total tick period will be tatickinterval * 10 this long
         if(Result = ICD.tachycardia or (TaTime < 10 * TaTickInterval and TaTime > 0)) then
            if(TaTime mod TaTickInterval = 0) then
               --basic unit to impuse is 2
               ImpulseGenerator.SetImpulse(Generator, 2);
               --After 10 times, tachycardia finished  , set tatime to 0, reset 
               if(TaTime = 10 * TaTickInterval) then
                  TaTime := 0;
                  Put_Line("TA FINISH");
                  New_Line;
               end if;
            else
               -- reset impulsegenerator to 0, during the interval
               ImpulseGenerator.SetImpulse(Generator, 0);   
            end if;
            TaTime := TaTime + 1;
            
         elsif(Result = ICD.ventricle) then
            --set joules when venreicle is detected
            ImpulseGenerator.SetImpulse(Generator, ICD.VENTRICLE_JOULES);
               
         else
            -- nothing happens, impulse 0
            ImpulseGenerator.SetImpulse(Generator, 0);
         end if;
         
         ImpulseGenerator.Tick(Generator, Hrt);          
      else
         --If the monitor is not on, return 0 for both values
         Icd_tor.Rate := Measures.BPM'First;
      end if;
      
   end Tick;    
end ICD;
