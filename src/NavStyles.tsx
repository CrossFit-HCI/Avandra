import { StyleSheet } from "react-native";

export const navContainerViewStyle = (isNavOpened: boolean) => {
    // Used to hide and show part of the Nav; this mimics the Nav being open or
    // closed.
    let navHeight = '50%';
    let navBottom = isNavOpened ? '0%' : '-42%';

    return StyleSheet.create({
        container: {
            height: navHeight,
            width: '100%',
            justifyContent: 'center',      
            alignItems: 'center',
            backgroundColor: 'lightgrey',
            position: 'absolute',
            bottom: navBottom,
        }
    });
}

export const navContainerBarViewStyle = StyleSheet.create({
    container: {
        flex: 1,
        width: '100%',
        justifyContent: 'center',      
        alignItems: 'flex-end',
        position: 'absolute',
        top: 2.5
    }
});