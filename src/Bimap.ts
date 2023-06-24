import Maybe, {nothing, just} from "./Maybe";

type Bimap<V> = V[]

export const add = <V>(bimap: Bimap<V>, value: V): Bimap<V> => {
    bimap.push(value);

    return bimap;
}



// Reimplement the functions below to make Bimap act like a bimap. 


/** A bidirectional map; a one-to-one correspondence between keys can values. */
// export default class Bimap {
//     /** The key-value store. */
//     private map: Entry[];
//     /** The value-key store. */
//     private mapInv: Entry[];
    
//     constructor(map?: Map<string,number>) {
//         if (map) {
//             map.forEach((value,key) => {
//                 this.map.push({key: key, value: value})
//             });
//             this.mapInv = new Map();
//             this.map.forEach((value,key) => {
//                 this.mapInv.set(value,key);
//             });
//         } else {
//             this.map = new Map<string,number>();
//             this.mapInv = new Map<number,string>();
//         }
//     }

//     private toObject() {
//         return {
//             map: this.map
//         };
//     }

//     serialize() {
//         return JSON.stringify(this.toObject());
//     }

//     static fromSerialize(serialized: string) {
//         const bimap: ReturnType<Bimap["toObject"]> = JSON.parse(serialized);

//         return new Bimap(bimap.map);
//     }

//     /**
//      * Add a new key-value pair to the map.
//      * @param key The key used to reference the value.
//      * @param value The value the key references.
//      */
//     set(key: string, value: number) {
//         this.map.set(key,value);
//         this.mapInv.set(value,key);
//     }

//     /**
//      * Delete a key-value pair from the map.
//      * @param key The key used to reference the value.
//      * @param value The value the key references.
//      * @returns `true` if the key-value pair were deleted, and `false` otherwise.
//      */
//     delete(key: string, value: number): boolean {
//         return (this.map.delete(key) && this.mapInv.delete(value));
//     }

//     /**
//      * Get the value referenced by `key`.
//      * @param key The key used to reference the value.
//      * @returns The value referenced by key if it exists.
//      */
//     getValue(key: string): Maybe<number> {
//         let value = this.map.get(key);

//         if (value != undefined) {
//             return just(value);
//         } else {
//             return nothing;
//         }
//     }

//     /**
//      * Get the key that references `value`.
//      * @param value The value referenced by a key.
//      * @returns The key that reference `value` if it exists.
//      */
//     getKey(value: number): Maybe<string> {
//         let key = this.mapInv.get(value);

//         if (key != undefined) {
//             return just(key);
//         } else {
//             return nothing;
//         }
//     }

//     /**
//      * Determines if a key exists in the map.
//      * @returns `true` if `key` exists in map, and `false` otherwise.
//      */
//     hasKey(key: string): boolean {
//         return this.map.has(key);
//     }

//     /**
//      * Determines if a value exists in the map.
//      * @returns `true` if `value` exists in map, and `false` otherwise.
//      */
//     hasValue(value: number): boolean {
//         return this.mapInv.has(value);
//     }
// }