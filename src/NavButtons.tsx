import React from "react";
import { closeNav, useAppDispatch } from "./NavViewModel";
import { Button, GestureResponderEvent } from "react-native";

/**
 * The type of props for Nav buttons.
 */
interface NavButtonProps {
    /** The title of the button. */
    title: string;
    /** The call back for when the button is pressed. */
    onPress: (event: GestureResponderEvent) => void;
}

/**
 * A button for navigating to a new screen.
 * @param props - A `NavButtonProps`.
 * @returns A RN Button with the onPress callback wrapped to manage the state of
 * the Nav properly.
 */
export const NaviButton = (props:NavButtonProps) => {
    const dispatch = useAppDispatch();
    const closeTheNav = () => dispatch(closeNav())
    
    const onPressCallback = (event: GestureResponderEvent) => {
        // Make sure the Nav is closed before transitioning to a new screen.
        closeTheNav();
        props.onPress(event);
    }

    return (
        <Button title={props.title} onPress={onPressCallback} />
    )
}

export const NextNavButton = () => {

}

export const GoBackNavButton = () => {

}

