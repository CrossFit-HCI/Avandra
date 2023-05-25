import Maybe, {nothing, just} from "./Maybe.js";

/** A bidirectional map; a one-to-one correspondence between keys can values. */
export default class Bimap<KeyT,ValueT> {
    /** The key-value store. */
    private map: Map<KeyT,ValueT>;
    /** The value-key store. */
    private mapInv: Map<ValueT,KeyT>;

    constructor() {
        this.map = new Map<KeyT,ValueT>();
        this.mapInv = new Map<ValueT,KeyT>();
    }

    /**
     * Add a new key-value pair to the map.
     * @param key The key used to reference the value.
     * @param value The value the key references.
     */
    set(key: KeyT, value: ValueT) {
        this.map.set(key,value);
        this.mapInv.set(value,key);
    }

    /**
     * Delete a key-value pair from the map.
     * @param key The key used to reference the value.
     * @param value The value the key references.
     * @returns `true` if the key-value pair were deleted, and `false` otherwise.
     */
    delete(key: KeyT, value: ValueT): boolean {
        return (this.map.delete(key) && this.mapInv.delete(value));
    }

    /**
     * Get the value referenced by `key`.
     * @param key The key used to reference the value.
     * @returns The value referenced by key if it exists.
     */
    getValue(key: KeyT): Maybe<ValueT> {
        let value = this.map.get(key);

        if (value != undefined) {
            return just(value);
        } else {
            return nothing;
        }
    }

    /**
     * Get the key that references `value`.
     * @param value The value referenced by a key.
     * @returns The key that reference `value` if it exists.
     */
    getKey(value: ValueT): Maybe<KeyT> {
        let key = this.mapInv.get(value);

        if (key != undefined) {
            return just(key);
        } else {
            return nothing;
        }
    }

    /**
     * Determines if a key exists in the map.
     * @returns `true` if `key` exists in map, and `false` otherwise.
     */
    hasKey(key: KeyT): boolean {
        return this.map.has(key);
    }

    /**
     * Determines if a value exists in the map.
     * @returns `true` if `value` exists in map, and `false` otherwise.
     */
    hasValue(value: ValueT): boolean {
        return this.mapInv.has(value);
    }
}