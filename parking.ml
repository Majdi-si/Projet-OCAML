module Vol = Vol

module ParkingHashtbl = Hashtbl.Make(struct
  type t = string
  let equal = String.equal
  let hash = Hashtbl.hash
end)

let parking_vols = ParkingHashtbl.create 10

let add_vol_to_parking parking vol =
  let vols = try ParkingHashtbl.find parking_vols parking with Not_found -> [] in
  ParkingHashtbl.replace parking_vols parking (vol :: vols)