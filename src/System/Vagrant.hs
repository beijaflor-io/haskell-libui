module System.Vagrant where

import           System.Environment
import           System.Process

vagrantUp :: FilePath -> IO String
vagrantUp fp = do
    l <- getEnvironment
    let l' = filter ((/= "VAGRANT_VAGRANTFILE") . fst) l
    readCreateProcess
        ((shell "vagrant up") { env = Just (("VAGRANT_VAGRANTFILE", fp):l')
                              })
        ""
