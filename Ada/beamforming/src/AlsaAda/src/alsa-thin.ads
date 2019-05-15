with Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
private package Alsa.Thin is
   use Interfaces.C;

   function Snd_Close (Item : Pcm_Device) return Alsa_Err_Code;
   pragma Import (C, Snd_Close, "snd_pcm_close");

   function Get_Channels (Param  : Configuration_Space;
                          Result : access Unsigned)
                          return Alsa_Err_Code;

   pragma Import (C, Get_Channels, "snd_pcm_hw_params_get_channels");

   function Get_Min_Channels (Param  : Configuration_Space;
                              Result : access Unsigned)
                              return Alsa_Err_Code;

   pragma Import (C, Get_Min_Channels, "snd_pcm_hw_params_get_channels_min");

   function Get_Max_Channels (Param  : Configuration_Space;
                              Result : access Unsigned)
                              return Alsa_Err_Code;

   pragma Import (C, Get_Max_Channels, "snd_pcm_hw_params_get_channels_max");

   function New_HW_Parameters (Ptr : access Configuration_Space)
                               return Alsa_Err_Code;
   pragma Import (C, New_HW_Parameters, "snd_pcm_hw_params_malloc");

   function Export_HW_Parameters (Dev    : Pcm_Device;
                                  Params : Configuration_Space)
                                  return Alsa_Err_Code;
   pragma Import (C, Export_HW_Parameters, "snd_pcm_hw_params_any");

   procedure Free_HW (Params : Configuration_Space);
   pragma Import (C, Free_HW, "snd_pcm_hw_params_free");

   function Test_Format (Dev   : Pcm_Device;
                         Param : Configuration_Space;
                         Fmt   : Data_Format)
                         return Alsa_Err_Code;
   pragma Import (C, Test_Format, "snd_pcm_hw_params_test_format");

   function Get_Format (Param : Configuration_Space;
                        Fmt   : out Data_Format)
                        return Alsa_Err_Code;
   pragma Import (C, get_Format, "snd_pcm_hw_params_get_format");


   function Get_Rate (Params : Configuration_Space;
                      Result : access Unsigned;
                      Dir    : access Int)
                      return Alsa_Err_Code;
   pragma Import (C, Get_Rate, "snd_pcm_hw_params_get_rate");

   function Get_Rate_Min (Params : Configuration_Space;
                          Result : access Unsigned;
                          Dir    : access Int)
                          return Alsa_Err_Code;
   pragma Import (C, Get_Rate_Min, "snd_pcm_hw_params_get_rate_min");

   function Get_Rate_Max (Params : Configuration_Space;
                          Result : access Unsigned;
                          Dir    : access Int)
                          return Alsa_Err_Code;
   pragma Import (C, Get_Rate_Max, "snd_pcm_hw_params_get_rate_max");

   function Set_Channels_Near (Dev : Pcm_Device;
                               Params : Configuration_Space;
                               N_Channels : in out Channel_Count)
                               return Alsa_Err_Code;

   pragma Import (C, Set_Channels_Near, "snd_pcm_hw_params_set_channels_near");

   function Set_Format (Dev    : Pcm_Device;
                        Params : Configuration_Space;
                        Format : Data_Format)
                        return Alsa_Err_Code;

   pragma Import (C, Set_Format, "snd_pcm_hw_params_set_format");

   function Set_Rate_Near (Dev          : Pcm_Device;
                           Params       : Configuration_Space;
                           Desired_Rate : in out Sampling_Rate;
                           Rounding     : in out Int)
                           return Alsa_Err_Code;

   pragma Import (C, Set_Rate_Near, "snd_pcm_hw_params_set_rate_near");

   function Set_Parameters (Dev    : Pcm_Device;
                            Params : Configuration_Space)
                            return Alsa_Err_Code;
   pragma Import (C, Set_Parameters, "snd_pcm_hw_params");

   function Set_Access (Dev    : Pcm_Device;
                        Params : Configuration_Space;
                        Mode   : Access_Mode )
                        return Alsa_Err_Code;
   pragma Import (C, Set_Access, "snd_pcm_hw_params_set_access");

   function Snd_Pcm_Open (Dev       : access Pcm_Device;
                          Name      : Chars_Ptr;
                          Direction : Stream_Direction;
                          Mode      : Int)
                          return Alsa_Err_Code;
   pragma Import (C, Snd_Pcm_Open, "snd_pcm_open");

   function Alsa_Read (Dev      : Pcm_Device;
                       Data     : System.Address;
                       N_Frames : Unsigned_Long)
                       return Alsa_Err_Code;

   pragma Import (C, Alsa_Read, "snd_pcm_readi");

   procedure Free (Item : Hint_Ptr);
   pragma Import (C, Free, "snd_device_name_free_hint");


end Alsa.Thin;
