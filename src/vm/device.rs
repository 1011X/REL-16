

pub const MAX_PORTS: usize = 256;

pub struct DeviceManager {
    ports: [u16; MAX_PORTS],
    
    /// Stores device and its starting port on the ports array.
    devices: Vec<(Device, usize)>,
}

impl DeviceManager {
    pub fn new() -> Self {
        DeviceManager {
            ports: [0; 256],
            devices: Vec::new(),
        }
    }
    
    pub fn add(&mut self, dev: Device) {
        let start = self.end();
        self.devices.push((dev, start));
    }
    
    pub fn call(&mut self, rev: bool, reg: &mut u16, port: u8) -> bool {
        let port = port as usize;
        std::mem::swap(reg, &mut self.ports[port]);
        
        for &mut (ref mut device, start) in &mut self.devices {
            let end = start + device.ports();
            if start <= port && port < end {
                device.io(rev, &mut self.ports[start..end]);
                return false;
            }
        }
        
        true
    }
    
    fn end(&self) -> usize {
        self.devices.iter().map(|(_, len)| len).sum()
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
                    
                    // unknown command, do nothing
                    (_, &mut [_, _]) => {}
                    
                    (_, _) => unreachable!()
                }
            }
            
            _ => unimplemented!()
        }
    }
}
