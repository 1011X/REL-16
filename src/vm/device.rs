

pub const MAX_PORTS: usize = 256;

pub struct DeviceManager {
    ports: [u16; MAX_PORTS],
    
    /// Stores device and its starting port on the ports array.
    devices: Vec<(Device, usize)>,
    status: u16,
}

impl DeviceManager {
    pub fn new() -> Self {
        DeviceManager {
            ports: [0; 256],
            devices: Vec::new(),
            status: 0,
        }
    }
    
    pub fn add(&mut self, dev: Device) {
        let start = self.end();
        self.devices.push((dev, start));
    }
    
    pub fn call(&mut self, rev: bool, reg: &mut u16, port: u8) -> bool {
        let port = port as usize;
        std::mem::swap(reg, &mut self.ports[port]);
        
        if port == 0 || port == 1 {
            self.ports[1] = match self.ports[0] {
                // do nothing
                0x0000 => 0,
                
                // return device manager status
                0x0001 => self.status,
                
                // return number of devices
                0x0002 => self.devices.len() as u16,
                
                // return type of device N
                0x0100..=0x01FF => {
                    let devn = (self.ports[0] & 0xFF) as usize;
                    
                    self.devices.get(devn)
                        .map(|(dev, _)| match dev {
                            Device::Stack(..)    => 1,
                            Device::Disk(..)     => 2,
                            Device::Keyboard     => 3,
                            Device::Display      => 4,
                        })
                        .unwrap_or(0)
                }
                
                // return start port of device N
                0x0200..=0x02FF => {
                    let devn = (self.ports[0] & 0xFF) as usize;
                    
                    self.devices.get(devn)
                        .map(|&(_, start)| start as u16)
                        .unwrap_or(0)
                }
                
                // return device width (number of ports) of device N
                0x0300..=0x03FF => {
                    let devn = (self.ports[0] & 0xFF) as usize;
                    
                    self.devices.get(devn)
                        .map(|(dev, _)| dev.ports() as u16)
                        .unwrap_or(0)
                }
                
                // anything else is unimplemented
                _ => unimplemented!()
            };
            
            return false;
        }
        else {
            for &mut (ref mut device, start) in &mut self.devices {
                let end = start + device.ports();
                
                if start <= port && port < end {
                    device.io(rev, &mut self.ports[start..end]);
                    return false;
                }
            }
        }
        
        true
    }
    
    fn end(&self) -> usize {
        // device manager uses first 2 ports to describe connected devices
        self.devices.iter().map(|(_, len)| len).sum::<usize>() + 2
    }
    
    pub fn debug_devices(&self) {
        println!("Devices: {:#?}", self.devices);
    }
}

#[derive(Debug)]
pub enum Device {
    Stack(Vec<u16>),
    Disk(Vec<u16>),
    Keyboard,
    Display,
}

impl Device {
    pub fn ports(&self) -> usize {
        match self {
            Device::Stack(..) => 2,
            _ => unimplemented!(),
        }
    }
    
    pub fn io(&mut self, reverse: bool, data: &mut [u16]) {
        match self {
            Device::Stack(store) => {
                // PUSH = 1
                // POP = 2
                // nothing = _
                match (reverse, data) {
                    // do nothing
                    (_, &mut [0, _]) => {}
                    
                    // push
                    (false, &mut [1, ref mut data]) |
                    (true,  &mut [2,  ref mut data]) => {
                        store.push(*data);
                        *data = 0;
                    }
                    
                    // pop
                    (false, &mut [2,  ref mut data]) |
                    (true,  &mut [1, ref mut data]) => {
                        *data = store.pop().unwrap();
                    }
                    
                    // unknown command, crash
                    (_, &mut [_, _]) => unimplemented!(),
                    
                    (_, _) => unreachable!()
                }
            }
            
            _ => unimplemented!()
        }
    }
}
